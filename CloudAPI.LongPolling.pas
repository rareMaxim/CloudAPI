unit CloudAPI.LongPolling;

interface

uses
  System.Classes;

type
  TcaLongPolling = class(TObject)
  private
    FPollingInterval: Integer;
    FIsActive: Boolean;
    FThread: TThread;
  protected
    procedure SetIsActive(const Value: Boolean);
    procedure Execute; virtual; abstract;
  public
    constructor Create; virtual;
    procedure Start;
    procedure Stop;
  public
    property IsActive: Boolean read FIsActive write SetIsActive;
    property PollingInterval: Integer read FPollingInterval write FPollingInterval default 1000;
  end;

implementation

uses
  System.SysUtils;

{ TcaLongPolling }

constructor TcaLongPolling.Create;
begin
  inherited Create;
  FPollingInterval := 1000;
end;

procedure TcaLongPolling.SetIsActive(const Value: Boolean);
begin
  if FIsActive = Value then
    Exit;
  FIsActive := Value;
  if FIsActive then
  begin
    FThread := TThread.CreateAnonymousThread(Execute);
    FThread.FreeOnTerminate := False;
    FThread.Start;
  end
  else
  begin
    FIsActive := False;
    FreeAndNil(FThread);
  end;
end;

procedure TcaLongPolling.Start;
begin
  IsActive := True;
end;

procedure TcaLongPolling.Stop;
begin
  IsActive := False;
end;

end.
