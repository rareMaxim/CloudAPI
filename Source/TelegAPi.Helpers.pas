unit TelegAPi.Helpers;

interface

uses
  TelegAPi.Bot,
  TelegAPi.Types,
  TelegAPi.Types.Enums,
  System.Classes,
  System.Net.Mime;

type
  TTelegramBotHelper = class helper for TTelegramBot
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
    procedure AddStream(const AFieldName, AFileName: string;  Data: TStream);
  end;

  TtgMessageHelper = class helper for TTgMessage
    function IsCommand(const AValue: string): Boolean;
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

procedure TtgTMultipartFormDataHelper.AddStream(const AFieldName, AFileName: string; Data: TStream);
var
  LFileStream  : TFileStream;
  LTmpDir      : string;
  LTmpFilename : string;
begin
  //get filename for tmp folder e.g. ..\AppData\local\temp\4F353A8AC6AB446D9F592A30B157291B
  LTmpDir      := IncludeTrailingPathDelimiter(TPath.GetTempPath)+TPath.GetGUIDFileName(false);
  LTmpFilename := IncludeTrailingPathDelimiter(LTmpDir)+ExtractFileName(AFileName);
  try
    TDirectory.CreateDirectory(LTmpDir);
    try
      LFileStream := TFileStream.Create(LTmpFilename, fmCreate);
      try
        LFileStream.CopyFrom(Data, 0);
      finally
        LFileStream.Free;
      end;
      AddFile(AFieldName, LTmpFilename);
    finally
      TFile.Delete(LTmpFilename);
    end;
  finally
    TDirectory.Delete(LTmpDir);
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

{ TtgMessageHelper }

function TtgMessageHelper.IsCommand(const AValue: string): Boolean;
var
  LEnt: TtgMessageEntity;
begin
  Result := False;
  if not Assigned(Self.Entities) then exit;

  for LEnt in Self.Entities do
    if (LEnt.TypeMessage = TtgMessageEntityType.bot_command) then
      if Text.Substring(LEnt.Offset, LEnt.Length).StartsWith(AValue, True) then
      begin
        LEnt.Free;
        Exit(True);
      end;
end;

end.

