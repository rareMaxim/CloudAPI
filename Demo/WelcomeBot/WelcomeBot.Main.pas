unit WelcomeBot.Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.SvcMgr,
  Vcl.Dialogs,
  TelegAPI.UpdateParser,
  TelegAPI.Receiver.Base,
  TelegAPI.Receiver.Service,
  TelegAPI.Base,
  TelegAPI.Bot.Impl,
  CrossUrl.SystemNet.HttpClient,
  TelegAPI.Types,
  TelegAPI.Logger,
  TelegAPI.Logger.Old,
  WelcomeBot.Core,
  WelcomeBot.Config;

type
  TWbMainSrv = class(TService)
    cuHttpClientSysNet1: TcuHttpClientSysNet;
    TelegramBot1: TTelegramBot;
    tgReceiverService1: TtgReceiverService;
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure tgReceiverService1Message(ASender: TObject; AMessage: ITgMessage);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
    FLogger: ILogger;
    FConfig: TwbConfig;
    FConfigPath: string;
    procedure ParseTextMessage(MSG: ITgMessage);
    procedure ParseServMsg(MSG: ITgMessage);
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  WbMainSrv: TWbMainSrv;

implementation

uses
  WelcomeBot.Logger,
  TelegAPI.Logger.Grijjy,
  TelegAPI.Helpers,
  TelegAPI.Types.Enums,
  TelegAPI.Utils.Json;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWORD); stdcall;
begin
  WbMainSrv.Controller(CtrlCode);
end;

function TWbMainSrv.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TWbMainSrv.ParseServMsg(Msg: ITgMessage);
var
  LText: string;
begin
  FLogger.Enter(Self, 'ParseServMsg');
  if Msg.NewChatMember <> nil then
  begin
    LText := FConfig.WelcomeText//
      .Replace('%Username%', Msg.From.Username) //
      .Replace('%LastName%', Msg.From.LastName) //
      .Replace('%FirstName%', Msg.From.FirstName) //
;
    if Msg.From.FirstName.IsEmpty and Msg.NewChatMember.LastName.IsEmpty then
      LText := LText.Replace('%NAMEorUSERNAME%', Msg.NewChatMember.Username)
    else
      LText := LText.Replace('%NAMEorUSERNAME%', Msg.NewChatMember.LastName +
        ' ' + Msg.NewChatMember.FirstName);
  end;

  TelegramBot1.SendMessage(Msg.Chat.ID, LText);
  FLogger.Leave(Self, 'ParseServMsg');
end;

procedure TWbMainSrv.ParseTextMessage(MSG: ITgMessage);
begin

end;

procedure TWbMainSrv.ServiceCreate(Sender: TObject);
begin
  FLogger := TtgGrijjyLogger.Create(Self);
  FConfigPath := ExtractFilePath(ParamStr(0)) + 'config.json';
  FConfig := TJsonUtils.FileToObject<TwbConfig>(FConfigPath);
  TelegramBot1.Token := FConfig.Token;
  TelegramBot1.Logger := FLogger;
end;

procedure TWbMainSrv.ServiceDestroy(Sender: TObject);
begin
  TJsonUtils.ObjectToFile(FConfig, FConfigPath);
  tgReceiverService1.Stop;
  FreeAndNil(FConfig);
end;

procedure TWbMainSrv.ServiceStart(Sender: TService; var Started: Boolean);
begin
  tgReceiverService1.Start;
  Started := True;
end;

procedure TWbMainSrv.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  tgReceiverService1.Stop;
  Stopped := True;
end;

procedure TWbMainSrv.tgReceiverService1Message(ASender: TObject; AMessage: ITgMessage);
begin
  TelegramBot1.Logger.Enter(Self, 'tgReceiverService1Message');
  if AMessage.&Type = TtgMessageType.ServiceMessage then
    ParseServMsg(AMessage);
  TelegramBot1.Logger.Leave(Self, 'tgReceiverService1Message');
end;

end.

