unit caMacMidiForm;

{$mode objfpc}{$H+}

{$linkframework CoreMidi}

interface

uses
  Classes, ExtCtrls, SpinEx, StdCtrls, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, CheckLst, MacOsAll,
  caDbg, caMidi, caMidiIntf, caMidiTypes;

type

  { TMainForm }

  TcaMainForm = class(TForm)
    CCLabel: TLabel;
    ErrorsLabel:TLabel;
    ErrorsMemo:TMemo;
    PGMLabel: TLabel;
    PGMSpin: TSpinEditEx;
    SendButton: TButton;
    MidiInDevices: TCheckListBox;
    MidiOutDevices: TCheckListBox;
    ButtonPanel: TPanel;
    CCSpin: TSpinEditEx;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
  private
    { private declarations }
    FOutputPort: longword;
    function CreateMIDIOutputPort(Client: longword): boolean;
    function GetDestination(Index: integer; out Destination: MIDIEndpointRef): boolean;
    procedure SendCCMidiMessage(Channel, CCValue: byte);
    procedure SendPGMMidiMessage(Channel, PGMValue: byte);
    procedure SendMidiPacket(Destination: MIDIEndpointRef; Packet: MIDIPacket);
  public
    { public declarations }
  end;

var
  MainForm: TcaMainForm;

implementation

{$R *.lfm}

{ Stand-alone procedures }

procedure midiInputCallback(pktlist: MIDIPacketListPtr; readProcRefCon: Pointer; srcConnRefCon: Pointer); mwpascal;
begin
end;

{ TcaMainForm }

procedure TcaMainForm.FormActivate(Sender: TObject);
var
  midiClient: longword = 0;
  Result: OSStatus;
  inputPort: longword = 0;
begin
  Midi.GetDevices(ioIn, MidiInDevices.Items);
  Midi.GetDevices(ioOut, MidiOutDevices.Items);

  ToDo!

  Result := MIDIClientCreate(CFSTR('MIDI CLIENT'), nil, nil, midiClient);
  if (Result <> noErr) then
    ShowMessage('Error creating MIDI client: ' + GetMacOSStatusErrorString(Result) + '  ' + GetMacOSStatusCommentString(Result));

  Result := MIDIInputPortCreate(midiClient, CFSTR('Input'), @midiInputCallback, nil, inputPort);
  if (Result <> noErr) then
    ShowMessage('Error creating MIDI input port: ' + GetMacOSStatusErrorString(Result) + '  ' + GetMacOSStatusCommentString(Result));

  if not CreateMIDIOutputPort(midiClient) then
    ShowMessage('Failed to create MIDI output port');
end;

procedure TcaMainForm.FormCreate(Sender: TObject);
begin
  FOutputPort := 0;
end;

procedure TcaMainForm.FormDestroy(Sender: TObject);
begin
end;

procedure TcaMainForm.SendButtonClick(Sender: TObject);
begin
  SendCCMidiMessage(0, CCSpin.Value and $FF);
  SendPGMMidiMessage(0, PGMSpin.Value and $FF);
end;

function TcaMainForm.CreateMIDIOutputPort(Client: longword): boolean;
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

function TcaMainForm.GetDestination(Index: integer; out Destination: MIDIEndpointRef): boolean;
begin
  Destination := MIDIGetDestination(MidiOutDevices.ItemIndex);
  Result := True;
  if Destination = 0 then
  begin
    ShowMessage('No MIDI Destination available');
    Result := False;
  end;
end;

procedure TcaMainForm.SendCCMidiMessage(Channel, CCValue: byte);
var
  Destination: MIDIEndpointRef;
  Packet: MIDIPacket;
begin
  if GetDestination(MidiOutDevices.ItemIndex, Destination) then
  begin
    // Build packet
    Packet.TimeStamp := 0; // Send immediately
    Packet.length := 3;
    Packet.Data[0] := $B0 or (Channel and $0F); // CC status
    Packet.Data[1] := 0;  // CC number (0 in this case)
    Packet.Data[2] := CCValue;
    // Send MIDI data
    SendMidiPacket(Destination, Packet);
  end;
end;

procedure TcaMainForm.SendPGMMidiMessage(Channel, PGMValue: byte);
var
  Destination: MIDIEndpointRef;
  Packet: MIDIPacket;
begin
  if GetDestination(MidiOutDevices.ItemIndex, Destination) then
  begin
    // Build packet
    Packet.TimeStamp := 0; // Send immediately
    Packet.length := 2;
    Packet.Data[0] := $C0 or (Channel and $0F); // Program Change status
    Packet.Data[1] := PGMValue;  // CC number (0 in this case)
    // Send MIDI data
    SendMidiPacket(Destination, Packet);
  end;
end;

procedure TcaMainForm.SendMidiPacket(Destination: MIDIEndpointRef; Packet: MIDIPacket);
var
  Result: OSStatus;
  PacketList: MIDIPacketList;
begin
  // Build packetlist
  PacketList.numPackets := 1;
  PacketList.packet[0] := Packet;

  Result := MIDISend(FOutputPort, Destination, PacketList);
  if Result <> noErr then
    ShowMessage('Error sending MIDI message: ' + GetMacOSStatusErrorString(Result) + '  ' + GetMacOSStatusCommentString(Result));
end;

end.
