
using { /Fortnite.com/Devices }
using { /UnrealEngine.com/Temporary/Diagnostics }

device_script_log := class(log_channel){}

# Your new device script
my_device_script := class<concrete>(creative_device):

    DeviceScriptLog:log = log{Channel := device_script_log}

    OnBegin<override>()<suspends>:void=
        #TODO: Place logic to run when the device enters play mode here
        DeviceScriptLog.Print("Hello Devices!")