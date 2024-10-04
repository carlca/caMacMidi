unit caMidiIntf;

interface

uses
  Classes, SysUtils, caMidiTypes;

type
  IcaMidiInterface = interface
    procedure GetDevices(InOut: TcaMidiInOut; Devices: TStrings);
    procedure Initialize;
    procedure Finalize;
    procedure SendCCMessage(Channel, Controller, Value: Byte);
    procedure SendPGMMessage(Channel, PGM: Byte);
  end;

implementation

end.


