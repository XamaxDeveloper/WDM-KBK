#include "Header.h"
_CRT_SECURE_NO_WARNINGS
typedef
struct _KD_EXTENSION
{
	PDEVICE_OBJECT	m_pLowerDeviceObject;
	PFILE_OBJECT	m_pLowerFileObject;
} KD_EXTENSION, *PKD_EXTENSION;

volatile LONG gSysEnters = 0;
volatile BOOLEAN g_bUnload = FALSE;
PDEVICE_OBJECT g_pDeviceObject = NULL;
PDEVICE_OBJECT g_pControlDeviceObject = NULL;

VOID DriverUnload(IN PDRIVER_OBJECT DriverObject)
{
	DbgPrint("GooBay");
	DriverObject;
}
//NTSTATUS DriverShutdown(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
//{
//}
NTSTATUS ReadCompletion(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp, IN PVOID Context)
{
	DbgPrint("ReadCompletion");
	Context;
	PKEYBOARD_INPUT_DATA KeyData;
	ULONG_PTR KeyCount;
	ULONG i;

	if (Irp->IoStatus.Status == STATUS_SUCCESS)
	{
		KeyData = (PKEYBOARD_INPUT_DATA)Irp->AssociatedIrp.SystemBuffer;
		KeyCount = Irp->IoStatus.Information / sizeof(KEYBOARD_INPUT_DATA);

		for (i = 0; i < KeyCount; i++)
		{
			DbgPrint("kd4.sys: key SCANCODE:%d ", KeyData[i].MakeCode);

			if (KeyData[i].Flags == KEY_MAKE)
				DbgPrint("pressed");

			if (KeyData[i].Flags & KEY_BREAK)
				DbgPrint("released");
			if (KeyData[i].Flags & KEY_E0)
				DbgPrint(" E0 flag");
			if (KeyData[i].Flags & KEY_E1)
				DbgPrint(" E1 flag");

			DbgPrint("\n");
		}
	}

	if (Irp->PendingReturned)
		IoMarkIrpPending(Irp);

	ObDereferenceObject(DeviceObject);
	InterlockedDecrement((PLONG)&gSysEnters);

	return STATUS_SUCCESS;
}


NTSTATUS Dispatch(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
	DbgPrint("Dispatch");
	PIO_STACK_LOCATION IrpSp = IoGetCurrentIrpStackLocation(Irp);

	// необрабатываемые запросы на наш управл€ющий девайс(MJ_CREATE..) завершаем с успехом
	if (IrpSp->DeviceObject == g_pControlDeviceObject)
	{
		Irp->IoStatus.Information = 0;
		Irp->IoStatus.Status = STATUS_SUCCESS;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);

		return STATUS_SUCCESS;
	}
	DbgPrint("Dispatch_2");
	if (!g_bUnload)
	{
		switch (IrpSp->MajorFunction)
		{
		case IRP_MJ_READ:
			// чтоб драйвер не выгрузилс€ до конца CompletionRoutine
			ObReferenceObject(DeviceObject);
			InterlockedIncrement((PLONG)&gSysEnters);

			IoCopyCurrentIrpStackLocationToNext(Irp);
			IoSetCompletionRoutine(Irp, ReadCompletion, NULL, TRUE, TRUE, TRUE);

			break;
		default:
			IoSkipCurrentIrpStackLocation(Irp);
		}
	}
	else
	{
		IoSkipCurrentIrpStackLocation(Irp);
	}
	DbgPrint("Dispatch_3");
	return IoCallDriver(((PKD_EXTENSION)DeviceObject->DeviceExtension)->m_pLowerDeviceObject, Irp);
}


NTSTATUS  DriverEntry(IN PDRIVER_OBJECT DriverObject, IN PUNICODE_STRING RegistryPath)
{
	DbgPrint("Hello");
	RegistryPath;
	NTSTATUS status;
	UNICODE_STRING cntdKbdClassStr;
	PFILE_OBJECT fileObject;
	PDEVICE_OBJECT lowerDeviceObject;
	PDEVICE_OBJECT deviceObject;
	PDEVICE_OBJECT controlDeviceObject;
	UNICODE_STRING cntdControlSymlinkName;
	int i;
	// повесим все на один хэндлер..
	for (i = 0; i <= IRP_MJ_MAXIMUM_FUNCTION; i++)
		DriverObject->MajorFunction[i] = Dispatch;
	//// кроме DeviceIoControl'а..
	//DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = DeviceControl;
	// и shutdown'а
	//DriverObject->MajorFunction[IRP_MJ_SHUTDOWN] = DriverShutdown;
	DriverObject->DriverUnload = DriverUnload;

	//RtlInitUnicodeString(&cntdControlDeviceName, CONTROL_DEVICE_NAME);//инициализаци€ подсчитанной строки ёникод.
	DbgPrint("CreateEntity");
	status = IoCreateDevice(DriverObject, 0, NULL, FILE_DEVICE_UNKNOWN, 0, FALSE, &controlDeviceObject);
	if (!NT_SUCCESS(status))
		return status;

	controlDeviceObject->Flags |= DO_BUFFERED_IO;
	
	DbgPrint("GetDeviceObject");
	RtlInitUnicodeString(&cntdKbdClassStr, L"\\Device\\KeyboardClass0");
	status = IoGetDeviceObjectPointer(&cntdKbdClassStr, 0, &fileObject, &lowerDeviceObject);
	if (!NT_SUCCESS(status))
	{
		IoDeleteSymbolicLink(&cntdControlSymlinkName);
		IoDeleteDevice(controlDeviceObject);
		return STATUS_UNSUCCESSFUL;
	}
	DbgPrint("CreateDevice");
	// безым€нный девайс..
	status = IoCreateDevice(DriverObject, 0, NULL, FILE_DEVICE_KEYBOARD, 0,	FALSE, &deviceObject);
	if (!NT_SUCCESS(status))
	{
		IoDeleteSymbolicLink(&cntdControlSymlinkName);
		IoDeleteDevice(controlDeviceObject);
		ObDereferenceObject(fileObject);

		return STATUS_UNSUCCESSFUL;
	}
	deviceObject->DeviceExtension = lowerDeviceObject;
	deviceObject->DeviceExtension = fileObject;
	((PKD_EXTENSION)deviceObject->DeviceExtension)->m_pLowerDeviceObject = lowerDeviceObject;
	((PKD_EXTENSION)deviceObject->DeviceExtension)->m_pLowerFileObject = fileObject;
	// keyboardclassx использует buffered io
	deviceObject->Flags |= DO_BUFFERED_IO;
	// аттачимс€ в стек..
	if (!IoAttachDeviceToDeviceStack(deviceObject, lowerDeviceObject))
	{
		IoDeleteSymbolicLink(&cntdControlSymlinkName);
		IoDeleteDevice(controlDeviceObject);
		IoDeleteDevice(deviceObject);
		ObDereferenceObject(fileObject);

		return STATUS_UNSUCCESSFUL;
	}

	// регистрим шатдаун нотификацию на контрол-девайс
	status = IoRegisterShutdownNotification(controlDeviceObject);
	if (!NT_SUCCESS(status))
	{
		IoDetachDevice(lowerDeviceObject);
		IoDeleteSymbolicLink(&cntdControlSymlinkName);
		IoDeleteDevice(controlDeviceObject);
		IoDeleteDevice(deviceObject);
		ObDereferenceObject(fileObject);

		return STATUS_UNSUCCESSFUL;
	}

	// сохран€ем девайсы глобально дл€ доступа из Unload'ов
	/*g_pDeviceObject = pDeviceObject;
	g_pControlDeviceObject = pControlDeviceObject;*/

	deviceObject->Flags &= ~DO_DEVICE_INITIALIZING;
	controlDeviceObject->Flags &= ~DO_DEVICE_INITIALIZING;

	DbgPrint("kd4.sys: loaded..\n");

	return STATUS_SUCCESS;
}
