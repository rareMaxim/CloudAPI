unit TelegAPI.Receiver.Base;

interface

uses
  System.Classes,
  System.SysUtils,
  TelegAPI.UpdateParser,
  TelegAPI.Bot,
  TelegAPI.Bot.Impl,
  TelegAPI.Types,
  TelegAPI.Types.Enums;

type
  TTgBotReceiverBase = class(TTgBotUpdateParser)
  private
    FBotDonor: TTelegramBot;
    FAllowedUpdates: TAllowedUpdates;
    FMessageOffset: Int64;
    FPollingInterval: Integer;
    FThread: TThread;
    FIsActive: Boolean;
    procedure SetIsActive(const AValue: Boolean);
    procedure AfterCreate;
  protected
    function ReadUpdates: TArray<ItgUpdate>; virtual;
    procedure Go; virtual;
    // События
    procedure DoOnStart; virtual; abstract;
    procedure DoOnStop; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(ABot: TTelegramBot); reintroduce; overload;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    [Default(False)]
    property IsActive: Boolean read FIsActive write SetIsActive;
  published
    property Bot: TTelegramBot read FBotDonor write FBotDonor;
    [Default(0)]
    property MessageOffset: Int64 read FMessageOffset write FMessageOffset;
    property AllowedUpdates: TAllowedUpdates read FAllowedUpdates write FAllowedUpdates default UPDATES_ALLOWED_ALL;
    [Default(1000)]
    property PollingInterval: Integer read FPollingInterval write FPollingInterval;
  end;

implementation

uses
  CloudAPI.Exception;

{ TTgBotReceiverBase }

constructor TTgBotReceiverBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AfterCreate;
end;

procedure TTgBotReceiverBase.AfterCreate;
begin
  MessageOffset := 0;
  AllowedUpdates := UPDATES_ALLOWED_ALL;
  PollingInterval := 1000;
end;

constructor TTgBotReceiverBase.Create(ABot: TTelegramBot);
begin
  inherited Create(nil);
  AfterCreate;
  FBotDonor := ABot;
end;

destructor TTgBotReceiverBase.Destroy;
begin
  Stop;
  inherited;
end;

procedure TTgBotReceiverBase.Go;
var
  LUpdates: TArray<ItgUpdate>;
begin
  DoOnStart;
  while FIsActive do
  begin
    LUpdates := ReadUpdates;
    if Length(LUpdates) = 0 then
    begin
      Sleep(FPollingInterval);
      Continue;
    end;
    MessageOffset := LUpdates[High(LUpdates)].ID + 1;
    EventParser(LUpdates);
    Sleep(FPollingInterval);
  end;
  DoOnStop;
end;

function TTgBotReceiverBase.ReadUpdates: TArray<ItgUpdate>;
var
  LBot: TTelegramBot;
begin
  LBot := TTelegramBot.Create(Self);
  try
    FBotDonor.AssignTo(LBot);
    Result := LBot.GetUpdates(MessageOffset, 100, 0, AllowedUpdates);
  finally
    LBot.Free;
  end;
end;

procedure TTgBotReceiverBase.SetIsActive(const AValue: Boolean);
begin
  if FIsActive = AValue then
    Exit;
  FIsActive := AValue;
  if AValue then
  begin
    FThread := TThread.CreateAnonymousThread(Go);
    FThread.FreeOnTerminate := False;
    FThread.Start;
  end
  else
    FreeAndNil(FThread);
end;

procedure TTgBotReceiverBase.Start;
begin
  IsActive := True;
end;

procedure TTgBotReceiverBase.Stop;
begin
  IsActive := False;
end;

end.

