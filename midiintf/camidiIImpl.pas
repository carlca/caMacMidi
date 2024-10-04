unit caMidiImpl;

interface

uses
  Classes, SysUtils,
  {$IFDEF DARWIN}
  uMidiMac,
  {$ENDIF}
  {$IFDEF WINDOWS}
  uMidiWin,
  {$ENDIF}
  {$IFDEF LINUX}
  uMidiLinux,
  {$ENDIF}
  uMidiIntf;

type

  TMidiImpl = class
  protected
    FIntf: IMidiInterface;
  public
    constructor Create;
    destructor Destroy; override;
    property Intf: IMidiInterface read FIntf;
  end;

implementation

constructor TMidiImpl.Create;
begin
  inherited Create;
  {$IFDEF DARWIN}
  FIntf := TMidiMac.Create;
  {$ENDIF}
  {$IFDEF WINDOWS}
  uMidiWin,
  {$ENDIF}
  {$IFDEF LINUX}
  uMidiLinux,
  {$ENDIF}
end;

destructor TMidiImpl.Destroy;
begin
  FIntf := nil;
  inherited Destroy;
end;

end.

