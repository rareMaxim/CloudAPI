unit TelegAPI.Bot;

interface

uses
  TelegAPI.Types,
  System.Generics.Collections,
  System.Rtti,
  System.Classes;

Type
  TTelegaBotOnUpdate = procedure(Const Sender: TObject; Const Update: TTelegaUpdate) of Object;
  TTelegaBorOnError = procedure(Const Sender: TObject; Const Code: Integer; Const Message: String)
    of Object;

  TTelegramBot = Class(TComponent)
  private
    FToken: String;
    FOnUpdate: TTelegaBotOnUpdate;
    FIsReceiving: Boolean;
    FUploadTimeout: Integer;
    FPollingTimeout: Integer;
    FMessageOffset: Integer;
    FOnError: TTelegaBorOnError;
    function IfThen(Value: Boolean; IfTrue: String; IfFalse: String): String;
    /// <summary>Монитор слежки за обновлениями</summary>
    procedure SetIsReceiving(const Value: Boolean);
  protected
    /// <summary>Мастер-функция для запросов на сервак</summary>
    Function API<T>(Const Method: String; Const Params: TDictionary<String, TValue>): T;
    /// <summary>Мастер-функция отправки сообщений </summary>
    Function SendMessage(MsgType: TTelegaMessageType; chatId: TValue; content: TValue;
      replyToMessageId: Integer = 0; replyMarkup: TTelegaReplyMarkup = nil;
      additionalParameters: TDictionary<String, TValue> = nil): TTelegaMessage;
  public
    /// <summary>A simple method for testing your bot's auth token.</summary>
    /// <returns>Returns basic information about the bot in form of a User object.</returns>
    Function getMe: TTelegaUser;
    /// <summary>Use this method to receive incoming updates using long polling. An Array of Update objects is returned.</summary>
    /// <param name="offset">Identifier of the first update to be returned. Must be greater by one than the highest among the identifiers of previously received updates. By default, updates starting with the earliest unconfirmed update are returned. An update is considered confirmed as soon as getUpdates is called with an offset higher than its update_id. The negative offset can be specified to retrieve updates starting from -offset update from the end of the updates queue. All previous updates will forgotten. </param>
    /// <param name="limit">Limits the number of updates to be retrieved. Values between 1—100 are accepted. Defaults to 100. </param>
    /// <param name="timeout">Timeout in seconds for long polling. Defaults to 0, i.e. usual short polling</param>
    Function getUpdates(Const offset: Integer = 0; Const limit: Integer = 100;
      Const timeout: Integer = 0): TArray<TTelegaUpdate>;
    Function sendTextMessage(Const chat_id: TValue; text: String;
      disableWebPagePreview: Boolean = false; replyToMessageId: Integer = 0;
      replyMarkup: TTelegaReplyMarkup = nil; ParseMode: TTelegaParseMode = TTelegaParseMode.Default;
      additionalParameters: TDictionary<String, TValue> = nil): TTelegaMessage;

    Function sendPhoto(chatId: TValue; photo: TValue; caption: string = '';
      replyToMessageId: Integer = 0; replyMarkup: TTelegaReplyMarkup = nil): TTelegaMessage;
    Function sendAudio(chat_id: TValue; audio: TValue; duration: Integer = 0;
      performer: String = ''; title: String = ''; disable_notification: Boolean = false;
      reply_to_message_id: Integer = 0; replyMarkup: TTelegaReplyMarkup = nil): TTelegaMessage;

    //
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(Const Token: String); overload;
  published
    { x } property UploadTimeout: Integer read FUploadTimeout write FUploadTimeout default 60000;
    { x } property PollingTimeout: Integer read FPollingTimeout write FPollingTimeout default 1000;
    property MessageOffset: Integer read FMessageOffset write FMessageOffset default 0;
    property IsReceiving: Boolean read FIsReceiving write SetIsReceiving default false;
    property Token: String read FToken write FToken;
    property OnUpdate: TTelegaBotOnUpdate read FOnUpdate write FOnUpdate;
    property OnError: TTelegaBorOnError read FOnError write FOnError;
  End;

implementation

uses
  XSuperObject,
  System.Net.Mime,
  System.Threading,
  System.SysUtils,
  System.Net.HttpClient,
  System.Net.URLClient;

Function TTelegaMessageToKeyValue(Value: TTelegaMessageType): TPair<String, String>;
Begin
  case Value of
    TTelegaMessageType.TextMessage:
      Result := TPair<String, String>.Create('sendMessage', 'text');
    TTelegaMessageType.PhotoMessage:
      Result := TPair<String, String>.Create('sendPhoto', 'photo');
    TTelegaMessageType.AudioMessage:
      Result := TPair<String, String>.Create('sendAudio', 'audio');
    TTelegaMessageType.VideoMessage:
      Result := TPair<String, String>.Create('sendVideo', 'video');
    TTelegaMessageType.VoiceMessage:
      Result := TPair<String, String>.Create('sendVoice', 'voice');
    TTelegaMessageType.DocumentMessage:
      Result := TPair<String, String>.Create('sendDocument', 'document');
    TTelegaMessageType.StickerMessage:
      Result := TPair<String, String>.Create('sendSticker', 'sticker');
    TTelegaMessageType.LocationMessage:
      Result := TPair<String, String>.Create('sendLocation', 'latitude');
    TTelegaMessageType.ContactMessage:
      Result := TPair<String, String>.Create('sendContact', 'contact');
    TTelegaMessageType.VenueMessage:
      Result := TPair<String, String>.Create('sendVenue', 'venue');
  else
    raise Exception.Create('Мы такое не сделали =(');
  end
End;

Function ToModeString(Mode: TTelegaParseMode): String;
Begin
  case Mode of
    TTelegaParseMode.Default:
      Result := '';
    TTelegaParseMode.Markdown:
      Result := 'Markdown';
    TTelegaParseMode.Html:
      Result := 'HTML';
  end;
End;

{ TTelegram }
function TTelegramBot.API<T>(const Method: String; Const Params: TDictionary<String, TValue>): T;
var
  Http: THTTPClient;
  content: String;
  Response: TTelegaApiResponse<T>;
  parameter: TPair<String, TValue>;
  Form: TMultipartFormData;
begin
  Http := THTTPClient.Create;
  Form := TMultipartFormData.Create;
  try
    // Преобразовуем параметры в строку, если нужно
    if Assigned(Params) then
    Begin
      for parameter in Params do
      begin
        if parameter.Value.IsType<TTelegaReplyMarkup> then
        begin
          { TODO -oOwner -cGeneral : Проверить че за херня тут твориться }
        end
        else if parameter.Value.IsType<TTelegaFileToSend> then
        Begin
          { TODO -oOwner -cGeneral : Отправка файлов }
          Form.AddFile(parameter.Key, parameter.Value.AsType<TTelegaFileToSend>.FileName);
        End
        else
        begin
          if parameter.Value.IsType<string> then
            Form.AddField(parameter.Key, parameter.Value.AsString)
          else if parameter.Value.IsType<Int64> then
            Form.AddField(parameter.Key, parameter.Value.AsInt64.ToString)
          else if parameter.Value.IsType<Boolean> then
            Form.AddField(parameter.Key, IfThen(parameter.Value.AsBoolean, 'true', 'false'))
        end;
      end;

    End;
    content := Http.Post('https://api.telegram.org/bot' + FToken + '/' + Method, Form)
      .ContentAsString(TEncoding.UTF8);
    // else
    // Content := Http.Get(Uri.ToString).ContentAsString(TEncoding.UTF8);

    if content.Contains('502 Bad Gateway') then
    begin
      if Assigned(OnError) then
        OnError(Self, 502, 'Bad Gateway');
      Exit;
    end;
    Response := TTelegaApiResponse<T>.FromJSON(content);
    if Not Response.Ok then
    begin
      if Assigned(OnError) then
        OnError(Self, Response.Code, Response.Message);
      Exit;
    end;
    Result := Response.ResultObject;
  finally
    Http.Free;
  end;
end;

constructor TTelegramBot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Create(string.Empty);
end;

constructor TTelegramBot.Create(const Token: String);
begin
  FToken := Token;
  IsReceiving := false;
  UploadTimeout := 60000;
  PollingTimeout := 1000;
  MessageOffset := 0;
end;

function TTelegramBot.getMe: TTelegaUser;
begin
  Result := Self.API<TTelegaUser>('getMe', nil);
end;

function TTelegramBot.getUpdates(const offset, limit, timeout: Integer): TArray<TTelegaUpdate>;
var
  Params: TDictionary<String, TValue>;
begin
  Params := TDictionary<String, TValue>.Create;
  try
    Params.Add('offset', offset);
    Params.Add('limit', limit);
    Params.Add('timeout', timeout);
    Result := Self.API < TArray < TTelegaUpdate >> ('getUpdates', Params);
  finally
    Params.Free;
  end;
end;

function TTelegramBot.IfThen(Value: Boolean; IfTrue, IfFalse: String): String;
begin
  if Value then
    Result := IfTrue
  else
    Result := IfFalse;
end;

function TTelegramBot.sendAudio(chat_id, audio: TValue; duration: Integer; performer, title: String;
  disable_notification: Boolean; reply_to_message_id: Integer; replyMarkup: TTelegaReplyMarkup)
  : TTelegaMessage;
var
  Params: TDictionary<String, TValue>;
begin
  Params := TDictionary<String, TValue>.Create;
  try
    Params.Add('chat_id', chat_id);
    Params.Add('audio', audio);
    Params.Add('duration', duration);
    Params.Add('performer', performer);
    Params.Add('title', title);
    Params.Add('disable_notification', disable_notification);
    Params.Add('reply_to_message_id', reply_to_message_id);
    Params.Add('replyMarkup', replyMarkup);
    Result := API<TTelegaMessage>('sendAudio', Params);
  finally
    Params.Free;
  end;
end;

function TTelegramBot.SendMessage(MsgType: TTelegaMessageType; chatId: TValue; content: TValue;
  replyToMessageId: Integer; replyMarkup: TTelegaReplyMarkup;
  additionalParameters: TDictionary<String, TValue>): TTelegaMessage;
var
  KeyValue: TPair<String, String>;
begin
  if Not Assigned(additionalParameters) then
    additionalParameters := TDictionary<String, TValue>.Create;

  KeyValue := TTelegaMessageToKeyValue(MsgType);

  additionalParameters.Add('chat_id', chatId);
  additionalParameters.Add('reply_markup', replyMarkup);

  if replyToMessageId <> 0 then
    additionalParameters.Add('reply_to_message_id', replyToMessageId);

  if NOT string.IsNullOrEmpty(KeyValue.Value) then
    additionalParameters.Add(KeyValue.Value, content);
  Result := API<TTelegaMessage>(KeyValue.Key, additionalParameters);
end;

function TTelegramBot.sendPhoto(chatId: TValue; photo: TValue; caption: string;
  replyToMessageId: Integer; replyMarkup: TTelegaReplyMarkup): TTelegaMessage;
var
  additionalParameters: TDictionary<string, TValue>;
begin
  additionalParameters := TDictionary<string, TValue>.Create();
  try
    additionalParameters.Add('caption', caption);
    Result := SendMessage(TTelegaMessageType.PhotoMessage, chatId.ToString, photo, replyToMessageId,
      replyMarkup, additionalParameters);
  finally
    additionalParameters.Free;
  end;
end;

function TTelegramBot.sendTextMessage(const chat_id: TValue; text: String;
  disableWebPagePreview: Boolean; replyToMessageId: Integer; replyMarkup: TTelegaReplyMarkup;
  ParseMode: TTelegaParseMode; additionalParameters: TDictionary<String, TValue>): TTelegaMessage;
begin
  if NOT Assigned(additionalParameters) then
    additionalParameters := TDictionary<string, TValue>.Create();
  if disableWebPagePreview then
    additionalParameters.Add('disable_web_page_preview', true);
  if ParseMode <> TTelegaParseMode.Default then
    additionalParameters.Add('parse_mode', ToModeString(ParseMode));

  Result := SendMessage(TTelegaMessageType.TextMessage, chat_id, text, replyToMessageId,
    replyMarkup, additionalParameters);
end;

procedure TTelegramBot.SetIsReceiving(const Value: Boolean);
var
  Task: ITask;
begin
  // Наверное надо бы синхронизацию добавить еще на события...
  FIsReceiving := Value;
  if (NOT Assigned(OnUpdate)) or (NOT FIsReceiving) then
    Exit;
  Task := TTask.Create(
    procedure
    var
      LUpdates: TArray<TTelegaUpdate>;

    Begin
      while FIsReceiving do
      Begin
        Sleep(PollingTimeout);
        LUpdates := getUpdates(MessageOffset, 100, PollingTimeout);
        TThread.Synchronize(nil,
          procedure
          var
            Update: TTelegaUpdate;
          begin
            for Update in LUpdates do
            begin
              OnUpdate(Self, Update);
              MessageOffset := Update.Id + 1;
            end;
          end);
      end;
    end);
  Task.Start;
end;

end.
