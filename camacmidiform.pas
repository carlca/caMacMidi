unit caMacMidiForm;

{$mode objfpc}{$H+}

{$linkframework CoreMidi}

interface

uses
  Classes, ExtCtrls, SpinEx, StdCtrls, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, CheckLst,
  caMidi, caMidiIntf, caMidiTypes;

type

  { TMainForm }

  TcaMainForm = class(TForm)
    CCLabel: TLabel;
    MidiIntfCheck: TCheckBox;
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
  public
    { public declarations }
  end;

var
  MainForm: TcaMainForm;

implementation

{$R *.lfm}

{ TcaMainForm }

procedure TcaMainForm.FormActivate(Sender: TObject);

begin
  Midi.GetDevices(ioIn, MidiInDevices.Items);
  Midi.GetDevices(ioOut, MidiOutDevices.Items);
end;

procedure TcaMainForm.SendButtonClick(Sender: TObject);
begin
  Midi.SendCC(MidiOutDevices.ItemIndex, 0, CCSpin.Value and $FF, ErrorsMemo.Lines);
  Midi.SendPGM(MidiOutDevices.ItemIndex, 0, PGMSpin.Value and $FF, ErrorsMemo.Lines);
end;

end.
