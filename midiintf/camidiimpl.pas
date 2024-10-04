unit caMidiImpl;

interface

uses
  Classes, SysUtils, caMidiIntf;

type

  TcaMidiImpl = class
  protected
    FIntf: IcaMidiInterface;
  public
    constructor Create;
    destructor Destroy; override;
    property Intf: IcaMidiInterface read FIntf;
  end;

implementation

uses
  {$IFDEF DARWIN}
  caMidiMac;
  {$ENDIF}
  {$IFDEF WINDOWS}
  caMidiWin;
  {$ENDIF}
  {$IFDEF LINUX}
  caMidiLinux;
  {$ENDIF}


constructor TcaMidiImpl.Create;
begin
  inherited Create;
  {$IFDEF DARWIN}
  FIntf := TcaMidiMac.Create;
  {$ENDIF}
  {$IFDEF WINDOWS}
  uMidiWin,
  {$ENDIF}
  {$IFDEF LINUX}
  uMidiLinux,
  {$ENDIF}
end;

destructor TcaMidiImpl.Destroy;
begin
  FIntf := nil;
  inherited Destroy;
end;

end.

