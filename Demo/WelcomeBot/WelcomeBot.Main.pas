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
  TelegAPI.Exceptions,
  TelegAPI.Base,
  TelegAPI.Bot.Impl,
  CrossUrl.SystemNet.HttpClient,
  TelegAPI.Types;

type
  TWbMain = class(TService)
    cuHttpClientSysNet1: TcuHttpClientSysNet;
    TelegramBot1: TTelegramBot;
    tgExceptionManagerUI1: TtgExceptionManagerUI;
    tgReceiverService1: TtgReceiverService;
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure tgReceiverService1Message(ASender: TObject; AMessage: ITgMessage);
  private
    { Private declarations }
    procedure ParseTextMessage(MSG: ITgMessage);
    procedure ParseServiceMessage(MSG: ITgMessage);
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  WbMain: TWbMain;

implementation

uses
  Grijjy.CloudLogging, //https://github.com/grijjy/GrijjyCloudLogger
  WelcomeBot.Config,
  WelcomeBot.Logger,
  TelegAPI.Helpers,
  TelegAPI.Types.Enums;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWORD); stdcall;
begin
  WbMain.Controller(CtrlCode);
end;

function TWbMain.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TWbMain.ParseServiceMessage(Msg: ITgMessage);
var
  LText: string;
begin
  LText := BOT_CONF.WelcomeText//
    .Replace('%Username%', Msg.From.Username) //
    .Replace('%LastName%', Msg.From.LastName) //
    .Replace('%FirstName%', Msg.From.FirstName) //
;
  if Msg.From.FirstName.IsEmpty and Msg.From.LastName.IsEmpty then
    LText := LText.Replace('%NAMEorUSERNAME%', Msg.From.Username)
  else
    LText := LText.Replace('%NAMEorUSERNAME%', Msg.From.LastName + ' ' + Msg.From.FirstName);
  if Assigned(Msg.NewChatMember) or (Assigned(Msg.NewChatMembers) and (Length(Msg.NewChatMembers)
    > 0)) then
  begin
    TelegramBot1.SendMessage(Msg.Chat.ID, LText);
  end;
end;

procedure TWbMain.ParseTextMessage(Msg: ITgMessage);
begin

end;

procedure TWbMain.ServiceStart(Sender: TService; var Started: Boolean);
begin
  GrijjyLog.EnterMethod(Self, 'ServiceStart');
  TelegramBot1.Token := BOT_CONF.Token;
  Started := TelegramBot1.IsValidToken;
  if Started then
  begin
    Log('Token is Valid', 'info', False);
  end
  else
  begin
    Log('Token is not Valid', 'ERROR', True);
    Exit;
  end;
  tgReceiverService1.Start;
  GrijjyLog.ExitMethod(Self, 'ServiceStart');
end;

procedure TWbMain.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Log('Service Stop', 'info', False);
  tgReceiverService1.Stop;
end;

procedure TWbMain.tgReceiverService1Message(ASender: TObject; AMessage: ITgMessage);
begin
  Log(AMessage.Text, 'input msg', False);
  case AMessage.&Type of
    TtgMessageType.TextMessage:
      ParseTextMessage(AMessage);
    TtgMessageType.ServiceMessage:
      ParseServiceMessage(AMessage);
  end;
end;

end.

