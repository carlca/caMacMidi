unit uMacMidi;

{$mode objfpc}{$H+}

{$linkframework CoreMidi}

interface

uses
  Classes, ExtCtrls, SpinEx, StdCtrls, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, CheckLst, MacOsAll,
  uDbg;

type

  { TMainForm }

  TMainForm = class(TForm)
    CCLabel: TLabel;
    PGMLabel: TLabel;
    PGMSpin: TSpinEditEx;
    SendButton: TButton;
    MidiInDevices: TCheckListBox;
    MidiOutDevices: TCheckListBox;
    ButtonPanel: TPanel;
    CCSpin: TSpinEditEx;
    procedure FormActivate(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
  private
    { private declarations }
    FOutputPort: longword;
    function CFStringToStr(AString: CFStringRef): string;
    function CreateMIDIOutputPort(Client: longword): boolean;
    procedure PopulateDeviceList(ListBox: TCheckListBox; in_out: string);
    procedure SendMIDIMessage(Destination: MIDIEndpointRef; status, Data1, Data2: byte);
    procedure SendCombinedCCAndPGM(Channel, CCValue, PGMValue: byte);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ Stand-alone procedures }

procedure midiInputCallback(pktlist: MIDIPacketListPtr; readProcRefCon: Pointer; srcConnRefCon: Pointer); mwpascal;
begin
end;

{ TMainForm }

procedure TMainForm.FormActivate(Sender: TObject);
var
  midiClient: longword;
  Result: OSStatus;
  inputPort: longword;
begin
  PopulateDeviceList(MidiInDevices, 'MidiIn');
  PopulateDeviceList(MidiOutDevices, 'MidiOut');

  Result := MIDIClientCreate(CFSTR('MIDI CLIENT'), nil, nil, midiClient);
  if (Result <> noErr) then
    ShowMessage('Error creating MIDI client: ' + GetMacOSStatusErrorString(Result) + '  ' + GetMacOSStatusCommentString(Result));

  Result := MIDIInputPortCreate(midiClient, CFSTR('Input'), @midiInputCallback, nil, inputPort);
  if (Result <> noErr) then
    ShowMessage('Error creating MIDI input port: ' + GetMacOSStatusErrorString(Result) + '  ' + GetMacOSStatusCommentString(Result));

  if not CreateMIDIOutputPort(midiClient) then
    ShowMessage('Failed to create MIDI output port');
end;

procedure TMainForm.SendButtonClick(Sender: TObject);
begin
  SendCombinedCCAndPGM(0, CCSpin.Value and $FF, PGMSpin.Value and $FF);
end;

function TMainForm.CFStringToStr(AString: CFStringRef): string;
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

function TMainForm.CreateMIDIOutputPort(Client: longword): boolean;
var
  Status: OSStatus;
begin
  Status := MIDIOutputPortCreate(Client, CFSTR('Output'), FOutputPort);
  if Status <> noErr then
  begin
    ShowMessage('Error creating MIDI output port: ' + GetMacOSStatusErrorString(Status) + '  ' + GetMacOSStatusCommentString(Status));
    Result := False;
  end;
  Result := True;
end;

procedure TMainForm.PopulateDeviceList(ListBox: TCheckListBox; in_out: string);
var
  Count, Index: integer;
  Source: MIDIEndpointRef;
  PDevName: CFStringRef;
  DevName: string;
begin
  ListBox.Clear;
  Count := specialize IfThen<integer>(In_Out = 'MidiIn', MIDIGetNumberOfSources, MIDIGetNumberOfDestinations);
  if Count > 0 then
  begin
    for Index := 0 to Count - 1 do
    begin
      Source := specialize IfThen<MIDIEndpointRef>(In_Out = 'MidiIn', MIDIGetSource(Index), MIDIGetDestination(Index));
      MIDIObjectGetStringProperty(Source, kMIDIPropertyName, PDevName);
      DevName := CFStringToStr(PDevName);
      if DevName <> '' then
        ListBox.items.Add(DevName);
    end;
  end;
end;

procedure TMainForm.SendMIDIMessage(Destination: MIDIEndpointRef; Status, Data1, Data2: byte);
var
  Packet: MIDIPacket;
  PacketList: MIDIPacketList;
  Result: OSStatus;
begin
  ShowMessage('SendMIDIMessage: ' + Status.ToString + ' ' + Data1.ToString + ' ' + Data2.ToString);
  Packet.TimeStamp := 0; // Send immediately
  Packet.length := 3;
  Packet.Data[0] := Status;
  Packet.Data[1] := Data1;
  Packet.Data[2] := Data2;

  PacketList.numPackets := 1;
  PacketList.packet[0] := Packet;

  Result := MIDISend(FOutputPort, Destination, PacketList);
  if Result <> noErr then
    ShowMessage('Error sending MIDI message: ' + GetMacOSStatusErrorString(Result) + '  ' + GetMacOSStatusCommentString(Result));
end;

procedure TMainForm.SendCombinedCCAndPGM(Channel, CCValue, PGMValue: byte);
var
  Destination: MIDIEndpointRef;
  Packet: MIDIPacket;
  PacketList: MIDIPacketList;
  Result: OSStatus;
begin
  Destination := MIDIGetDestination(MidiOutDevices.ItemIndex);
  if Destination = 0 then
  begin
    ShowMessage('No MIDI Destination available');
    Exit;
  end;

  Packet.TimeStamp := 0; // Send immediately
  Packet.length := 5;
  Packet.Data[0] := $B0 or (Channel and $0F); // CC status
  Packet.Data[1] := 0;  // CC number (0 in this case)
  Packet.Data[2] := CCValue;
  Packet.Data[3] := $C0 or (Channel and $0F); // Program Change status
  Packet.Data[4] := PGMValue;

  PacketList.numPackets := 1;
  PacketList.packet[0] := Packet;

  Result := MIDISend(FOutputPort, Destination, PacketList);
  if Result <> noErr then
    ShowMessage('Error sending MIDI message: ' + GetMacOSStatusErrorString(Result) + '  ' + GetMacOSStatusCommentString(Result));
end;

end.
