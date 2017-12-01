unit LoggerPro.TelegramAppender;
{ <@abstract(The unit to include if you want to use @link(TLoggerProRedisAppender))
  @author(Sysoev Maxim) }

interface

uses
  LoggerPro, System.Classes, System.DateUtils, TelegAPI.Bot, TelegAPi.Types;

type
  {
    @abstract(Logs sending message to your telegram)
    To learn how to use this appender, check the sample @code(\Extensions\Demo\telegram_appender\telegram_appender.dpr)
    @author(Sysoev Maxim - maks4a@gmail.com)
  }
  TLoggerProTelegramAppender = class(TLoggerProAppenderBase)
  private
    FBot: ITelegramBot;
    FToIDorUsername: string;
    FFrom: string;
  protected
    /// <summary>
    /// Ovveride this method in descendant if you want a different formatting for
    /// message subject or body
    /// </summary>
    procedure PrepareMessage(const aLogItem: TLogItem; out aSubject, aBody: string); virtual;
  public
    constructor Create(const aToken, aFrom, aToIDorUsername: string); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    procedure TryToRestart(var Restarted: Boolean); override;
  end;

implementation

uses
  System.SysUtils, TelegaPi.Factory, TelegaPi.Types.Enums;

const
  DEFAULT_LOG_FORMAT = '%0:s [TID %1:-8d][%2:-8s] %3:s [%4:s]';

constructor TLoggerProTelegramAppender.Create(const aToken, aFrom, aToIDorUsername: string);
begin
  inherited Create;
  FBot := TtgFactory.CreateTelegram(aToken);
  FToIDorUsername := aToIDorUsername;
  FFrom := aFrom;
  { by default, appender sends only errors }
  SetLogLevel(TLogType.Error);
end;

procedure TLoggerProTelegramAppender.PrepareMessage(const aLogItem: TLogItem; out aSubject, aBody: string);
begin
  aSubject := 'LoggerPro ' + aLogItem.LogTypeAsString.ToUpper + ' [' + aLogItem.LogTag + ']';
  aBody := Format(DEFAULT_LOG_FORMAT, [DateTimeToStr(aLogItem.TimeStamp), aLogItem.ThreadID, aLogItem.LogTypeAsString, aLogItem.LogMessage, aLogItem.LogTag]);
end;

procedure TLoggerProTelegramAppender.Setup;
begin
  //
end;

procedure TLoggerProTelegramAppender.TearDown;
begin
  try
    FBot := nil;
  except
    // do nothing
  end;
end;

procedure TLoggerProTelegramAppender.TryToRestart(var Restarted: Boolean);
begin
  inherited;
  Restarted := True;
end;

procedure SendMsg(aBot: ITelegramBot; const aFrom, aToAddresses: string; const Subject, Body: string);
const
  MSG_TMPL = ''//
    + 'From _%S_' + #13#10  //
    + 'Subject: *%S*' + #13#10  //
    + 'Body: ```%s```'//
;
var
  Msg: string;
begin
  Msg := Format(MSG_TMPL, [aFrom, Subject, Body]);
  aBot.SendMessage(aToAddresses, Msg, TtgParseMode.Markdown);
end;

procedure TLoggerProTelegramAppender.WriteLog(const aLogItem: TLogItem);
var
  lBody: string;
  lSubject: string;
begin
  PrepareMessage(aLogItem, lSubject, lBody);
  SendMsg(FBot,FFrom, FToIDorUsername, lSubject, lBody);
end;

end.

