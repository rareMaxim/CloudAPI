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
  ITgBotReceiverBase = interface
    ['{98A444BC-D8E3-4542-B75F-3A7AACFCAE74}']
    procedure Start;
    procedure Stop;
  end;

  TTgBotReceiverBase = class(TTgBotUpdateParser, ITgBotReceiverBase)
  private
    FBotDonor: TTelegramBot;
    FAllowedUpdates: TAllowedUpdates;
    FMessageOffset: Int64;
    FPollingInterval: Integer;
    FThread: TThread;
    FIsActive: Boolean;
    procedure SetIsActive(const AValue: Boolean);
  protected
    function ReadUpdates: TArray<ItgUpdate>; virtual;
    procedure Go; virtual;
    //События
    procedure DoOnStart; virtual; abstract;
    procedure DoOnStop; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(ABot: ITelegramBot); reintroduce; overload;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    [Default(False)]
    property IsActive: Boolean read FIsActive write SetIsActive;
  published
    property Bot: TTelegramBot read FBotDonor write FBotDonor;
    [Default(0)]
    property MessageOffset: Int64 read FMessageOffset write FMessageOffset;
    property AllowedUpdates: TAllowedUpdates read FAllowedUpdates write
      FAllowedUpdates default UPDATES_ALLOWED_ALL;
    [Default(1000)]
    property PollingInterval: Integer read FPollingInterval write FPollingInterval;
  end;

implementation


{ TTgBotReceiverBase }

constructor TTgBotReceiverBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MessageOffset := 0;
  AllowedUpdates := UPDATES_ALLOWED_ALL;
  PollingInterval := 1000;
end;

constructor TTgBotReceiverBase.Create(ABot: ITelegramBot);
begin
  Self.Create(nil);
  FBotDonor := ABot as TTelegramBot;
end;

destructor TTgBotReceiverBase.Destroy;
begin
 // FBotDonor.Free;
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
  except
    on E: Exception do
      Bot.ExceptionManager.HaveGlobalException('TTgBotReceiverBase.ReadUpdates', E)
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

