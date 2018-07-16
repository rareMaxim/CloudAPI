unit CloudAPI.Types;

interface

uses
  System.Net.Mime,
  System.Net.URLClient,
  System.Generics.Collections,
  System.SysUtils,
  System.Classes;

type
{$SCOPEDENUMS ON}
  TFileToSendTag = (ERROR = 254, ID = 0, FromURL = 1, FromFile = 2, FromStream = 3);
{$SCOPEDENUMS OFF}

  TFileToSend = record
  public
    Data: string;
    Content: TStream;
    Tag: TFileToSendTag;
{$REGION 'operator overload'}
    class operator Equal(a, b: TFileToSend): Boolean;
    class operator Implicit(const AValue: string): TFileToSend;
    class operator Implicit(AValue: TStream): TFileToSend;
{$ENDREGION}
    class function Create(const ATag: TFileToSendTag; const AData: string; AContent: TStream): TFileToSend; static;
    class function FromFile(const AFileName: string): TFileToSend; static;
    class function FromID(const AID: string): TFileToSend; static;
    class function FromURL(const AUrl: string): TFileToSend; static;
    class function FromStream(const AContent: TStream; const AFileName: string): TFileToSend; static;
    class function Empty: TFileToSend; static;
    function IsEmpty: Boolean;
  end;

  IRequestData = interface
    ['{CF275FE1-D684-45BF-84B8-2E809E58CB81}']
    function GetDomain: string;
    procedure SetDomain(const AValue: string);
    function GetStoreMultipartForm: TMultipartFormData;
    procedure SetStoreMultipartForm(const AValue: TMultipartFormData);
    function GetStoreStringList: TStringList;
    procedure SetStoreStringList(const Value: TStringList);
    function GetStoreUrl: TStringList;
    procedure SetStoreUrl(const Value: TStringList);
    function GetStoreHeaders: TList<TNetHeader>;
    procedure SetStoreHeaders(const Value: TList<TNetHeader>);

    //
    property Domain: string read GetDomain write SetDomain;
    property StoreMultipartForm: TMultipartFormData read GetStoreMultipartForm write SetStoreMultipartForm;
    property StoreStringList: TStringList read GetStoreStringList write SetStoreStringList;
    property StoreUrl: TStringList read GetStoreUrl write SetStoreUrl;
    property StoreHeaders: TList<TNetHeader> read GetStoreHeaders write SetStoreHeaders;
  end;

  TRequestData = class(TInterfacedObject, IRequestData)
  strict private
    FDomain: string;
    FStoreInFormData: TMultipartFormData;
    FStoreInStringList: TStringList;
    FStoreInUrl: TStringList;
    FStoreInHeader: TList<TNetHeader>;
  private
    function GetDomain: string;
    procedure SetDomain(const AValue: string);
    function GetStoreMultipartForm: TMultipartFormData;
    procedure SetStoreMultipartForm(const AValue: TMultipartFormData);
    function GetStoreStringList: TStringList;
    procedure SetStoreStringList(const Value: TStringList);
    function GetStoreUrl: TStringList;
    procedure SetStoreUrl(const Value: TStringList);
    function GetStoreHeaders: TList<TNetHeader>;
    procedure SetStoreHeaders(const Value: TList<TNetHeader>);
  public
    function ClearParams: IRequestData; virtual;
    constructor Create; virtual;
    destructor Destroy; override;
    property Domain: string read GetDomain write SetDomain;
    property StoreMultipartForm: TMultipartFormData read GetStoreMultipartForm write SetStoreMultipartForm;
    property StoreStringList: TStringList read GetStoreStringList write SetStoreStringList;
    property StoreUrl: TStringList read GetStoreUrl write SetStoreUrl;
    property StoreHeaders: TList<TNetHeader> read GetStoreHeaders write SetStoreHeaders;
  end;

implementation

{ TtgFileToSend }

class function TFileToSend.Create(const ATag: TFileToSendTag; const AData: string; AContent: TStream): TFileToSend;
begin
  Result.Tag := ATag;
  Result.Data := AData;
  Result.Content := AContent;
end;

class function TFileToSend.Empty: TFileToSend;
begin
  Result := TFileToSend.Create(TFileToSendTag.ERROR, '', nil);
end;

class operator TFileToSend.Equal(a, b: TFileToSend): Boolean;
begin
  Result := (a.Data = b.Data) and (a.Tag = b.Tag) and (a.Content = b.Content);
end;

class function TFileToSend.FromFile(const AFileName: string): TFileToSend;
begin
  if not FileExists(AFileName) then
    raise EFileNotFoundException.CreateFmt('File %S not found!', [AFileName]);
  Result := TFileToSend.Create(TFileToSendTag.FromFile, AFileName, nil);
end;

class function TFileToSend.FromID(const AID: string): TFileToSend;
begin
  Result := TFileToSend.Create(TFileToSendTag.ID, AID, nil);
end;

class function TFileToSend.FromStream(const AContent: TStream; const AFileName: string): TFileToSend;
begin
  // I guess, in most cases, AFilename param should contain a non-empty string.
  // It is odd to receive a file with filename and
  // extension which both are not connected with its content.
  if AFileName.IsEmpty then
    raise Exception.Create('TtgFileToSend: Filename is empty!');
  if not Assigned(AContent) then
    raise EStreamError.Create('Stream not assigned!');
  Result := TFileToSend.Create(TFileToSendTag.FromStream, AFileName, AContent);
end;

class function TFileToSend.FromURL(const AUrl: string): TFileToSend;
begin
  Result := TFileToSend.Create(TFileToSendTag.FromURL, AUrl, nil);
end;

class operator TFileToSend.Implicit(const AValue: string): TFileToSend;
begin
  Result.Content := nil;
  Result.Data := AValue;
  if FileExists(AValue) then
    Result.Tag := TFileToSendTag.FromFile
  else if AValue.contains('://') then
    Result.Tag := TFileToSendTag.FromURL
  else
    Result.Tag := TFileToSendTag.ID;
end;

class operator TFileToSend.Implicit(AValue: TStream): TFileToSend;
begin
  Result.Content := AValue;
  Result.Tag := TFileToSendTag.FromStream;
end;

function TFileToSend.IsEmpty: Boolean;
begin
  Result := Data.IsEmpty and not Assigned(Content);
end;

{ TRequestData }

function TRequestData.ClearParams: IRequestData;
begin
  FStoreInFormData.Free;
  FStoreInFormData := TMultipartFormData.Create;
  FStoreInStringList.Clear;
  FStoreInUrl.Clear;
  FStoreInHeader.Clear;
  Result := Self;
end;

constructor TRequestData.Create;
begin
  FStoreInFormData := TMultipartFormData.Create;
  FStoreInStringList := TStringList.Create;
  FStoreInUrl := TStringList.Create;
  FStoreInHeader := TList<TNetHeader>.Create;
end;

destructor TRequestData.Destroy;
begin
  FreeAndNil(FStoreInFormData);
  FreeAndNil(FStoreInStringList);
  FreeAndNil(FStoreInUrl);
  FreeAndNil(FStoreInHeader);
  inherited;
end;

function TRequestData.GetDomain: string;
begin
  Result := FDomain;
end;

function TRequestData.GetStoreHeaders: TList<TNetHeader>;
begin
  Result := FStoreInHeader;
end;

function TRequestData.GetStoreMultipartForm: TMultipartFormData;
begin
  Result := FStoreInFormData;
end;

function TRequestData.GetStoreStringList: TStringList;
begin
  Result := FStoreInStringList;
end;

function TRequestData.GetStoreUrl: TStringList;
begin
  Result := FStoreInUrl;
end;

procedure TRequestData.SetDomain(const AValue: string);
begin
  FDomain := AValue;
end;

procedure TRequestData.SetStoreHeaders(const Value: TList<TNetHeader>);
begin
  FStoreInHeader := Value;
end;

procedure TRequestData.SetStoreMultipartForm(const AValue: TMultipartFormData);
begin
  FStoreInFormData := AValue;
end;

procedure TRequestData.SetStoreStringList(const Value: TStringList);
begin
  FStoreInStringList := Value;
end;

procedure TRequestData.SetStoreUrl(const Value: TStringList);
begin
  FStoreInUrl := Value;
end;

end.

