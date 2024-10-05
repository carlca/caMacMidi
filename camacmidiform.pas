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
    MidiIntfCheck:TCheckBox;
    ErrorsLabel: TLabel;
    ErrorsMemo: TMemo;
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
  public
    { public declarations }
  end;

var
  MainForm: TcaMainForm;

implementation

{$R *.lfm}

{ TcaMainForm }

procedure TcaMainForm.FormActivate(Sender: TObject);
var
  MidiClient: longword;
begin
  Midi.GetDevices(ioIn, MidiInDevices.Items);
  Midi.GetDevices(ioOut, MidiOutDevices.Items);
  MidiClient := Midi.CreateMidiClient(ErrorsMemo.Lines);
  Midi.CreateMidiInputPort(MidiClient, ErrorsMemo.Lines);
  FOutputPort := Midi.CreateMidiOutputPort(MidiClient, ErrorsMemo.Lines);
end;

procedure TcaMainForm.SendButtonClick(Sender: TObject);
begin
  Midi.SendCC(MidiOutDevices.ItemIndex, FOutputPort, 0, CCSpin.Value and $FF, ErrorsMemo.Lines);
  Midi.SendPGM(MidiOutDevices.ItemIndex, FOutputPort, 0, PGMSpin.Value and $FF, ErrorsMemo.Lines);
end;

end.
