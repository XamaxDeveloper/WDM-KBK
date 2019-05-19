#include <ntddk.h>
#include <NTDDK.h>
#include <stdio.h>
#include <ntddkbd.h>

PDEVICE_OBJECT KeyboardDevice;
PDEVICE_OBJECT KeyboardFilterDevice;
PFILE_OBJECT   KeyboardFileObject;

NTSTATUS AddDevice(PDRIVER_OBJECT DriverObject, PDEVICE_OBJECT pdo);
VOID DriverUnload(IN PDRIVER_OBJECT DriverObject);

NTSTATUS DriverEntry(IN PDRIVER_OBJECT DriverObject, IN PUNICODE_STRING  RegistryPath)
{

	(void)DriverObject;
	(void)RegistryPath;
	DbgPrint("DriverEntryIN");

	DriverObject->DriverExtension->AddDevice = AddDevice;
	DriverObject->DriverUnload = DriverUnload;

	return STATUS_SUCCESS;
}

NTSTATUS AddDevice(PDRIVER_OBJECT DriverObject, PDEVICE_OBJECT pdo)
{
	CCHAR           NameBuffer[64];
	STRING          NameString;
	UNICODE_STRING  UnicodeString;

	sprintf(NameBuffer, "\\Device\\KeyboardClass0");
	RtlInitAnsiString(&NameString, NameBuffer);
	RtlAnsiStringToUnicodeString(&UnicodeString, &NameString, TRUE);

	NTSTATUS status = IoCreateDevice(DriverObject, 0, NULL,	FILE_DEVICE_MOUSE,	0,	FALSE,	&KeyboardFilterDevice);
	
	if (!NT_SUCCESS(status)) {
		DbgPrint("failed to create device\n");
		RtlFreeUnicodeString(&UnicodeString);
		return STATUS_SUCCESS;
	}

	KeyboardFilterDevice->Flags |= DO_BUFFERED_IO;

	status = IoGetDeviceObjectPointer(&UnicodeString,STANDARD_RIGHTS_ALL,&KeyboardFileObject,	&KeyboardDevice);

	if (!NT_SUCCESS(status)) {
		DbgPrint("failed to get top device object\n");
		IoDeleteDevice(KeyboardFilterDevice);
		RtlFreeUnicodeString(&UnicodeString);
		return STATUS_SUCCESS;
	}

	KeyboardDevice = IoAttachDeviceToDeviceStack(KeyboardFilterDevice,	pdo);

	if (KeyboardDevice == NULL) {
		DbgPrint("failed to attach device object\n");
	}

	RtlFreeUnicodeString(&UnicodeString);

	DbgPrint("Successfully connected to mouse device\n");

	return STATUS_SUCCESS;
}

VOID DriverUnload(IN PDRIVER_OBJECT DriverObject)
{
	(void)DriverObject;
	DbgPrint("unload driver\n");
}