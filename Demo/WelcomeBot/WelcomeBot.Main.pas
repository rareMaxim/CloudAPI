unit WelcomeBot.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  TelegAPI.UpdateParser, TelegAPI.Receiver.Base, TelegAPI.Receiver.Service,
  TelegAPI.Exceptions, TelegAPI.Base, TelegAPI.Bot.Impl,
  CrossUrl.SystemNet.HttpClient, TelegAPI.Types;

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
    procedure ParseTextMessage(Msg: ITgMessage);
    procedure ParseServiceMessage(Msg: ITgMessage);
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  WbMain: TWbMain;

implementation

uses
  WelcomeBot.Config,
  WelcomeBot.Logger,
  TelegAPI.Types.Enums;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  WbMain.Controller(CtrlCode);
end;

function TWbMain.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TWbMain.ParseServiceMessage(Msg: ITgMessage);
begin
  if Assigned(Msg.NewChatMember) or
    (Assigned(Msg.NewChatMembers) and (Length(Msg.NewChatMembers) > 0)) then
  begin

  end;
end;

procedure TWbMain.ParseTextMessage(Msg: ITgMessage);
begin
  TelegramBot1.SendMessage(Msg.From.ID, 'Test');
end;

procedure TWbMain.ServiceStart(Sender: TService; var Started: Boolean);
begin
  if BOT_CONF.Token.IsEmpty then
  begin
    Log('Token is empty', 'config', True);
  end;
  tgReceiverService1.Start;
end;

procedure TWbMain.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  tgReceiverService1.Stop;
end;

procedure TWbMain.tgReceiverService1Message(ASender: TObject;
  AMessage: ITgMessage);
begin
  case AMessage.&Type of
    TtgMessageType.TextMessage:
      ParseTextMessage(AMessage);
    TtgMessageType.ServiceMessage:
      ParseServiceMessage(AMessage);
  end;
end;

end.
