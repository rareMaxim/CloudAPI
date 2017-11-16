unit TelegAPI.Recesiver.Console;

interface

uses
  TelegAPI.Recesiver.Base,
  TelegAPI.Types,
  System.SysUtils;

type
  TtgRecesiverConsole = class(TTgBotRecesiverBase)
  private
    FOnStart: TProc;
    FOnStop: TProc;
    FOnUpdates: TProc<TArray<ItgUpdate>>;
    FOnUpdate: TProc<ItgUpdate>;
  protected
    procedure DoOnStart; override;
    procedure DoOnStop; override;
    procedure DoOnUpdates(AUpdates: TArray<ItgUpdate>); override;
    procedure DoOnUpdate(AUpdate: ItgUpdate); override;
  public
    property OnStart: TProc read FOnStart write FOnStart;
    property OnStop: TProc read FOnStop write FOnStop;
    property OnUpdates: TProc<TArray<ItgUpdate>> read FOnUpdates write FOnUpdates;
    property OnUpdate: TProc<ItgUpdate> read FOnUpdate write FOnUpdate;
  end;

implementation

{ TtgRecesiverConsole }

procedure TtgRecesiverConsole.DoOnStart;
begin
  inherited;
  if Assigned(OnStart) then
    OnStart();
end;

procedure TtgRecesiverConsole.DoOnStop;
begin
  inherited;
  if Assigned(OnStop) then
    OnStop();
end;

procedure TtgRecesiverConsole.DoOnUpdate(AUpdate: ItgUpdate);
begin
  inherited;
  if Assigned(OnUpdate) then
    OnUpdate(AUpdate);
end;

procedure TtgRecesiverConsole.DoOnUpdates(AUpdates: TArray<ItgUpdate>);
begin
  inherited;
  if Assigned(OnUpdates) then
    OnUpdates(AUpdates);
end;

end.

