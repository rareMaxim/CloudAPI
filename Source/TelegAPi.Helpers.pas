unit TelegAPi.Helpers;

interface

uses
  TelegAPi.Classes,
  System.Classes,
  System.Net.Mime;

type
  TtgParseModeHelper = record helper for TtgParseMode
    function ToString : string;
  end;

  TAllowedUpdatesHelper = record helper for TAllowedUpdates
    function ToString : string;
  end;

  TtgTMultipartFormDataHelper = Class helper for TMultipartFormData
    /// <summary>
    /// Add a form data Stream
    /// </summary>
    procedure AddStream(
      const AFieldName : string;
      Data             : TStream );
    experimental;
  End;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  System.Generics.Collections;

{ TtgParseModeHelper }

function TtgParseModeHelper.ToString : string;
  begin
    case Self of
      TtgParseMode.Default :
        Result := '';
      TtgParseMode.Markdown :
        Result := 'Markdown';
      TtgParseMode.Html :
        Result := 'HTML';
    end;
  end;

{ TAllowedUpdatesHelper }

function TAllowedUpdatesHelper.ToString : string;
  var
    LAllowed : TList< string >;
  begin
    Result := '[';
    LAllowed := TList< string >.Create;
    try
      if TAllowedUpdate.Message in Self
      then
        LAllowed.Add( '"message"' );
      if TAllowedUpdate.Edited_message in Self
      then
        LAllowed.Add( '"edited_message"' );
      if TAllowedUpdate.Channel_post in Self
      then
        LAllowed.Add( '"channel_post"' );
      if TAllowedUpdate.Edited_channel_post in Self
      then
        LAllowed.Add( '"edited_channel_post"' );
      if TAllowedUpdate.Inline_query in Self
      then
        LAllowed.Add( '"inline_query"' );
      if TAllowedUpdate.Chosen_inline_result in Self
      then
        LAllowed.Add( '"chosen_inline_result"' );
      if TAllowedUpdate.Callback_query in Self
      then
        LAllowed.Add( '"callback_query"' );
      Result := Result + Result.Join( ',', LAllowed.ToArray );
    finally
      LAllowed.Free;
    end;
    Result := Result + ']';
  end;

{ TtgTMultipartFormDataHelper }

procedure TtgTMultipartFormDataHelper.AddStream(
  const AFieldName : string;
  Data             : TStream );
  var
    lFileName : string;
    LFileStream : TFileStream;
  begin
    lFileName := TPath.GetTempFileName;
    try
      LFileStream := TFileStream.Create( lFileName, fmCreate );
      try
        LFileStream.CopyFrom( Data, 0 );
      finally
        LFileStream.Free;
      end;
      AddFile( AFieldName, lFileName );
    finally
      TFile.Delete( lFileName );
    end;
  end;

end.
