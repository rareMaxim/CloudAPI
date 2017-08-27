unit TelegAPi.Helpers;

interface

uses
  TelegAPi.Bot,
  TelegAPi.Types.Enums,
  System.Classes,
  System.Net.Mime;

type
  TTelegramBotHelper = class helper for TTelegramBotCore
    function IsValidToken: Boolean;
  end;

  TtgParseModeHelper = record helper for TtgParseMode
    function ToString: string;
  end;

  TAllowedUpdatesHelper = record helper for TAllowedUpdates
    function ToString: string;
  end;

  TSendChatActionHelper = record helper for TtgSendChatAction
    function ToString: string;
  end;

  TtgTMultipartFormDataHelper = class helper for TMultipartFormData
    /// <summary>
    ///   Add a form data Stream
    /// </summary>
    procedure AddStream(const AFieldName: string; Data: TStream);
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  System.Generics.Collections,
  System.RegularExpressions;

{ TtgParseModeHelper }

function TtgParseModeHelper.ToString: string;
begin
  case Self of
    TtgParseMode.Default:
      Result := '';
    TtgParseMode.Markdown:
      Result := 'Markdown';
    TtgParseMode.Html:
      Result := 'HTML';
  end;
end;

{ TAllowedUpdatesHelper }

function TAllowedUpdatesHelper.ToString: string;
var
  LAllowed: TList<string>;
begin
  LAllowed := TList<string>.Create;
  try
    if TAllowedUpdate.Message in Self then
      LAllowed.Add('"message"');
    if TAllowedUpdate.Edited_message in Self then
      LAllowed.Add('"edited_message"');
    if TAllowedUpdate.Channel_post in Self then
      LAllowed.Add('"channel_post"');
    if TAllowedUpdate.Edited_channel_post in Self then
      LAllowed.Add('"edited_channel_post"');
    if TAllowedUpdate.Inline_query in Self then
      LAllowed.Add('"inline_query"');
    if TAllowedUpdate.Chosen_inline_result in Self then
      LAllowed.Add('"chosen_inline_result"');
    if TAllowedUpdate.Callback_query in Self then
      LAllowed.Add('"callback_query"');
    Result := '[' + Result.Join(',', LAllowed.ToArray) + ']';
  finally
    LAllowed.Free;
  end;
end;

{ TtgTMultipartFormDataHelper }

procedure TtgTMultipartFormDataHelper.AddStream(const AFieldName: string; Data: TStream);
var
  lFileName: string;
  LFileStream: TFileStream;
begin
  lFileName := TPath.GetTempFileName;
  try
    LFileStream := TFileStream.Create(lFileName, fmCreate);
    try
      LFileStream.CopyFrom(Data, 0);
    finally
      LFileStream.Free;
    end;
    AddFile(AFieldName, lFileName);
  finally
    TFile.Delete(lFileName);
  end;
end;

{ TTelegramBotHelper }

function TTelegramBotHelper.IsValidToken: Boolean;
const
  TOKEN_CORRECT = '\d*:[\w\d-_]{35}';
begin
  Result := TRegEx.IsMatch(Token, TOKEN_CORRECT, [roIgnoreCase]);
end;

{ TSendChatActionHelper }

function TSendChatActionHelper.ToString: string;
begin
  case Self of
    TtgSendChatAction.Typing:
      Result := 'typing';
    TtgSendChatAction.UploadPhoto:
      Result := 'upload_photo';
    TtgSendChatAction.Record_video:
      Result := 'record_video';
    TtgSendChatAction.UploadVideo:
      Result := 'upload_video';
    TtgSendChatAction.Record_audio:
      Result := 'record_audio';
    TtgSendChatAction.Upload_audio:
      Result := 'upload_audio';
    TtgSendChatAction.Upload_document:
      Result := 'upload_document';
    TtgSendChatAction.Find_location:
      Result := 'find_location';
    TtgSendChatAction.Record_video_note:
      Result := 'record_video_note';
    TtgSendChatAction.Upload_video_note:
      Result := 'upload_video_note';
  end;
end;

end.

