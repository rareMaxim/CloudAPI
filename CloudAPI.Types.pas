unit CloudAPI.Types;

interface

uses
  System.Classes;

type
{$SCOPEDENUMS ON}
  TcaFileToSendTag = (Error = 254, Unknown = 0, ID = 100, FromURL = 101, FromFile = 102, FromStream = 103);
  TcaParameterType = (Cookie, GetOrPost, UrlSegment, HttpHeader, RequestBody, QueryString, QueryStringWithoutEncode);
  TcaMethod = (GET, POST, PUT, DELETE, HEAD, OPTIONS, PATCH, MERGE, COPY);
{$SCOPEDENUMS OFF}

  TcaFileToSend = record
  private
    FData: string;
    FContent: TStream;
    FTag: TcaFileToSendTag;
    FName: string;
  private
    class function TestString(const AValue: string): TcaFileToSendTag; static;
  public
    property Data: string read FData write FData;
    property Content: TStream read FContent write FContent;
    property Tag: TcaFileToSendTag read FTag write FTag;
    property Name: string read FName write FName;
{$REGION 'operator overload'}
    class operator Equal(a, b: TcaFileToSend): Boolean;
    class operator Implicit(const AValue: string): TcaFileToSend;
    class operator Implicit(AValue: TStream): TcaFileToSend;
{$ENDREGION}
    class function Create(const AData: string; AContent: TStream;
      const ATag: TcaFileToSendTag = TcaFileToSendTag.Unknown): TcaFileToSend; static;
    class function FromFile(const AFileName: string): TcaFileToSend; static;
    class function FromID(const AID: string): TcaFileToSend; static;
    class function FromURL(const AUrl: string): TcaFileToSend; static;
    class function FromStream(const AContent: TStream; const AFileName: string): TcaFileToSend; static;
    class function Empty: TcaFileToSend; static;
    function IsEmpty: Boolean;
  end;

  TcaRequestLimit = record
  private
    FIsGlobal: Boolean;
    FStartedAt: TDateTime;
    FEndingAt: TDateTime;
    FLimit: Int64;
    FName: string;
  public
    class function Create(const ALimit: Int64; const AName: string; const AIsGlobal: Boolean): TcaRequestLimit; static;
    property StartedAt: TDateTime read FStartedAt write FStartedAt;
    property EndingAt: TDateTime read FEndingAt write FEndingAt;
    property Limit: Int64 read FLimit write FLimit;
    property Name: string read FName write FName;
    function IsExpired: Boolean;
    function ActualLimit: Int64;
    property IsGlobal: Boolean read FIsGlobal write FIsGlobal;
    class function DatesDuration(const AAfter, ABefore: TDateTime): UInt64; static;
  end;

implementation

uses
  System.DateUtils,
  System.SysUtils;

{ TtgFileToSend }

class function TcaFileToSend.Create(const AData: string; AContent: TStream;
  const ATag: TcaFileToSendTag = TcaFileToSendTag.Unknown): TcaFileToSend;
begin
  Result.Tag := ATag;
  Result.Data := AData;
  Result.Content := AContent;
end;

class function TcaFileToSend.Empty: TcaFileToSend;
begin
  Result := TcaFileToSend.Create('', nil, TcaFileToSendTag.Error);
end;

class operator TcaFileToSend.Equal(a, b: TcaFileToSend): Boolean;
begin
  Result := (a.Data = b.Data) and (a.Tag = b.Tag) and (a.Content = b.Content);
end;

class function TcaFileToSend.FromFile(const AFileName: string): TcaFileToSend;
begin
  if not FileExists(AFileName) then
    raise EFileNotFoundException.CreateFmt('File %S not found!', [AFileName]);
  Result := TcaFileToSend.Create(AFileName, nil, TcaFileToSendTag.FromFile);
end;

class function TcaFileToSend.FromID(const AID: string): TcaFileToSend;
begin
  Result := TcaFileToSend.Create(AID, nil, TcaFileToSendTag.ID);
end;

class function TcaFileToSend.FromStream(const AContent: TStream; const AFileName: string): TcaFileToSend;
begin
  // I guess, in most cases, AFilename param should contain a non-empty string.
  // It is odd to receive a file with filename and
  // extension which both are not connected with its content.
  if AFileName.IsEmpty then
    raise Exception.Create('TtgFileToSend: Filename is empty!');
  if not Assigned(AContent) then
    raise EStreamError.Create('Stream not assigned!');
  Result := TcaFileToSend.Create(AFileName, AContent, TcaFileToSendTag.FromStream);
end;

class function TcaFileToSend.FromURL(const AUrl: string): TcaFileToSend;
begin
  Result := TcaFileToSend.Create(AUrl, nil, TcaFileToSendTag.FromURL);
end;

class operator TcaFileToSend.Implicit(const AValue: string): TcaFileToSend;
begin
  Result.Content := nil;
  Result.Data := AValue;
  Result.Tag := TestString(AValue);
end;

class operator TcaFileToSend.Implicit(AValue: TStream): TcaFileToSend;
begin
  Result.Content := AValue;
  Result.Tag := TcaFileToSendTag.FromStream;
end;

function TcaFileToSend.IsEmpty: Boolean;
begin
  Result := Data.IsEmpty and not Assigned(Content);
end;

class function TcaFileToSend.TestString(const AValue: string): TcaFileToSendTag;
begin
  if FileExists(AValue) then
    Result := TcaFileToSendTag.FromFile
  else if AValue.contains('://') then
    Result := TcaFileToSendTag.FromURL
  else
    Result := TcaFileToSendTag.ID;
end;

{ TcaRequestLimit }

function TcaRequestLimit.ActualLimit: Int64;
begin
  Result := DatesDuration(EndingAt, Now);
end;

class function TcaRequestLimit.Create(const ALimit: Int64; const AName: string; const AIsGlobal: Boolean)
  : TcaRequestLimit;
begin
  Result.Limit := ALimit;
  Result.Name := AName;
  Result.IsGlobal := AIsGlobal;
  Result.StartedAt := Now;
  Result.EndingAt := IncMilliSecond(Result.StartedAt, Result.Limit);
end;

class function TcaRequestLimit.DatesDuration(const AAfter, ABefore: TDateTime): UInt64;
begin
  Result := DateTimeToMilliseconds(AAfter - ABefore);
end;

function TcaRequestLimit.IsExpired: Boolean;
begin
  Result := Now > EndingAt;
end;

end.
