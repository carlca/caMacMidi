unit caMidiMac;

interface

uses
  Classes, SysUtils, MacOsAll, caMidiIntf, caMidiTypes;

type
  TcaMidiMac = class(TInterfacedObject, IcaMidiInterface)
  private
    function CFStringToStr(AString: CFStringRef): string;
  protected
    procedure GetDevices(InOut: TcaMidiInOut; Devices: TStrings);
    procedure Initialize;
    procedure Finalize;
    procedure SendCCMessage(Channel, Controller, Value: Byte);
    procedure SendPGMMessage(Channel, PGM: Byte);
  end;

implementation

function TcaMidiMac.CFStringToStr(AString: CFStringRef): string;
var
  Index: integer;
  Uni: UniChar;
begin
  if AString = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := '';
  for Index := 0 to CFStringGetLength(AString) - 1 do
  begin
    Uni := CFStringGetCharacterAtIndex(AString, Index);
    Result := Result + Chr(Uni);
  end;
  Result := AnsiToUtf8(Result);
end;

procedure TcaMidiMac.GetDevices(InOut: TcaMidiInOut; Devices: TStrings);
var
  Count, Index: integer;
  Source: MIDIEndpointRef;
  PDevName: CFStringRef;
  DevName: string;
begin
  Devices.Clear;
  Count := specialize IfThen<integer>(InOut = ioIn, MIDIGetNumberOfSources, MIDIGetNumberOfDestinations);
  if Count > 0 then
  begin
    for Index := 0 to Count - 1 do
    begin
      Source := specialize IfThen<MIDIEndpointRef>(InOut = ioIn, MIDIGetSource(Index), MIDIGetDestination(Index));
      MIDIObjectGetStringProperty(Source, kMIDIPropertyName, PDevName);
      DevName := CFStringToStr(PDevName);
      if DevName <> '' then
        Devices.Add(DevName);
    end;
  end;
end;

procedure TcaMidiMac.Initialize;
begin

end;

procedure TcaMidiMac.Finalize;
begin

end;

procedure TcaMidiMac.SendCCMessage(Channel, Controller, Value: Byte);
begin

end;

procedure TcaMidiMac.SendPGMMessage(Channel, PGM: Byte);
begin

end;

end.
