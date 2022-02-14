unit CloudAPI.Request;

interface

uses
  CloudAPI.Parameter,
  CloudAPI.Types,
  System.Generics.Collections,
  System.Net.UrlClient,
  System.Rtti, System.Classes;

type

  TcaParameterList = TList<TcaParameter>;
  TcaFileList = TList<TcaFileToSend>;

  IcaRequest = interface
    ['{13E72C25-63BE-4F99-8957-7DDA6810C28C}']
    // private
    function GetAlwaysMultipartFormData: Boolean;
    function GetDefaultParameterType: TcaParameterType;
    function GetFiles: TcaFileList;
    function GetMethod: TcaMethod;
    function GetCookies: TcaParameterList;
    function GetGetOrPosts: TcaParameterList;
    function GetHttpHeaders: TcaParameterList;
    function GetUrlSegments: TcaParameterList;
    function GetQueryString: TcaParameterList;
    function GetResource: string;
    procedure SetAlwaysMultipartFormData(const Value: Boolean);
    procedure SetDefaultParameterType(const Value: TcaParameterType);
    procedure SetMethod(const Value: TcaMethod);
    procedure SetResource(const Value: string);
    function GetRequestBody: TStringList;
    function GetStartAt: TDateTime;
    procedure SetStartAt(const Value: TDateTime);
    // public
    function AddParam(AParam: TcaParameter): IcaRequest; overload;
    function AddParam(const AName: string; AValue: TValue): IcaRequest; overload;
    function AddParam(const AName: string; AValue: TValue; AType: TcaParameterType): IcaRequest; overload;
    function AddHeader(const AName, AValue: string): IcaRequest;
    function AddHeaders(AHeaders: TArray < TPair < string, string >> ): IcaRequest;
    function AddCookie(const AName, AValue: string): IcaRequest;
    function AddUrlSegment(const AName, AValue: string): IcaRequest;
    function AddQueryParameter(const AName, AValue: string): IcaRequest; overload;
    function AddQueryParameterJoined(const ANameValue: string; ADelimeter: Char = '='): IcaRequest; overload;
    function AddQueryParameter(const AName, AValue: string; const AEncode: Boolean): IcaRequest; overload;
    function AddQueryParametersJoined(const ANameValues: string; ALineDelimeter: Char = '&'; ADelimeter: Char = '=')
      : IcaRequest; overload;
    procedure AddFile(const AFile: TcaFileToSend);
    function GetLimitInfo: TcaRequestLimit;
    procedure SetLimitInfo(const Value: TcaRequestLimit);
    // public
    function IsMultipartFormData: Boolean;
    function IsRequestBody: Boolean;
    property DefaultParameterType: TcaParameterType read GetDefaultParameterType write SetDefaultParameterType;
    property Files: TcaFileList read GetFiles;
    property AlwaysMultipartFormData: Boolean read GetAlwaysMultipartFormData write SetAlwaysMultipartFormData;
    property Resource: string read GetResource write SetResource;
    property Method: TcaMethod read GetMethod write SetMethod;
    property Cookies: TcaParameterList read GetCookies;
    property GetOrPosts: TcaParameterList read GetGetOrPosts;
    property HttpHeaders: TcaParameterList read GetHttpHeaders;
    property UrlSegments: TcaParameterList read GetUrlSegments;
    property QueryParameters: TcaParameterList read GetQueryString;
    property LimitInfo: TcaRequestLimit read GetLimitInfo write SetLimitInfo;
    property RequestBody: TStringList read GetRequestBody;
    property StartAt: TDateTime read GetStartAt write SetStartAt;
  end;

  TcaRequest = class(TInterfacedObject, IcaRequest)
  private
    FAlwaysMultipartFormData: Boolean;
    FDefaultParameterType: TcaParameterType;
    FMethod: TcaMethod;
    FResource: string;
    FHttpHeaders: TcaParameterList;
    FCookies: TcaParameterList;
    FGetOrPosts: TcaParameterList;
    FUrlSegments: TcaParameterList;
    FQueryStrings: TcaParameterList;
    FFiles: TcaFileList;
    FLimitInfo: TcaRequestLimit;
    FRequestBody: TStringList;
    FStartAt: TDateTime;
    function GetAlwaysMultipartFormData: Boolean;
    function GetDefaultParameterType: TcaParameterType;
    function GetFiles: TcaFileList;
    function GetMethod: TcaMethod;
    function GetResource: string;
    procedure SetAlwaysMultipartFormData(const Value: Boolean);
    procedure SetDefaultParameterType(const Value: TcaParameterType);
    procedure SetMethod(const Value: TcaMethod);
    procedure SetResource(const Value: string);
    function GetCookies: TcaParameterList;
    function GetGetOrPosts: TcaParameterList;
    function GetHttpHeaders: TcaParameterList;
    function GetLimitInfo: TcaRequestLimit;
    function GetUrlSegments: TcaParameterList;
    function GetQueryString: TcaParameterList;
    function GetRequestBody: TStringList;
    procedure SetLimitInfo(const Value: TcaRequestLimit);
    function GetStartAt: TDateTime;
    procedure SetStartAt(const Value: TDateTime);
  public
    constructor Create; overload;
    constructor Create(const AMethod: TcaMethod); overload;
    constructor Create(const AResource: string); overload;
    constructor Create(const AResource: string; const AMethod: TcaMethod); overload;
    constructor Create(const AResource: TUri; const AMethod: TcaMethod); overload;
    constructor Create(const AResource: TUri); overload;
    destructor Destroy; override;
    function AddParam(AParam: TcaParameter): IcaRequest; overload;
    function AddParam(const AName: string; AValue: TValue): IcaRequest; overload;
    function AddParam(const AName: string; AValue: TValue; AType: TcaParameterType): IcaRequest; overload;
    function AddHeader(const AName, AValue: string): IcaRequest;
    function AddHeaders(AHeaders: TArray < TPair < string, string >> ): IcaRequest;
    function AddCookie(const AName, AValue: string): IcaRequest;
    function AddUrlSegment(const AName, AValue: string): IcaRequest;
    function AddQueryParameter(const AName, AValue: string): IcaRequest; overload;
    function AddQueryParameterJoined(const ANameValue: string; ADelimeter: Char = '='): IcaRequest; overload;
    function AddQueryParameter(const AName, AValue: string; const AEncode: Boolean): IcaRequest; overload;
    function AddQueryParametersJoined(const ANameValues: string; ALineDelimeter: Char = '&'; ADelimeter: Char = '=')
      : IcaRequest; overload;
    procedure AddFile(const AFile: TcaFileToSend); overload;
    procedure AddFile(const AFile: TcaFileToSend; AParameterType: TcaParameterType); overload;
    function IsMultipartFormData: Boolean;
    function IsRequestBody: Boolean;
  public
    property DefaultParameterType: TcaParameterType read GetDefaultParameterType write SetDefaultParameterType;
    property Files: TcaFileList read GetFiles;
    property AlwaysMultipartFormData: Boolean read GetAlwaysMultipartFormData write SetAlwaysMultipartFormData;
    property Resource: string read GetResource write SetResource;
    property Method: TcaMethod read GetMethod write SetMethod;
    property Cookies: TcaParameterList read GetCookies;
    property GetOrPosts: TcaParameterList read GetGetOrPosts;
    property HttpHeaders: TcaParameterList read GetHttpHeaders;
    property UrlSegments: TcaParameterList read GetUrlSegments;
    property QueryParameters: TcaParameterList read GetQueryString;
    property LimitInfo: TcaRequestLimit read GetLimitInfo write SetLimitInfo;
    property RequestBody: TStringList read GetRequestBody;
    property StartAt: TDateTime read GetStartAt write SetStartAt;
  end;

implementation

uses
  CloudAPI.Exceptions,
  System.SysUtils;

{ TcaRequest }

constructor TcaRequest.Create;
begin
  inherited Create;
  FFiles := TcaFileList.Create;
  FHttpHeaders := TcaParameterList.Create;
  FCookies := TcaParameterList.Create;
  FGetOrPosts := TcaParameterList.Create;
  FUrlSegments := TcaParameterList.Create;
  FQueryStrings := TcaParameterList.Create;
  FRequestBody := TStringList.Create;
  FMethod := TcaMethod.GET;
end;

constructor TcaRequest.Create(const AMethod: TcaMethod);
begin
  Self.Create('', AMethod);
end;

destructor TcaRequest.Destroy;
begin
  FHttpHeaders.Free;
  FCookies.Free;
  FGetOrPosts.Free;
  FUrlSegments.Free;
  FQueryStrings.Free;
  FFiles.Free;
  FRequestBody.Free;
  inherited Destroy;
end;

function TcaRequest.AddCookie(const AName, AValue: string): IcaRequest;
begin
  Result := AddParam(AName, AValue, TcaParameterType.Cookie);
end;

procedure TcaRequest.AddFile(const AFile: TcaFileToSend);
begin
  AddFile(AFile, DefaultParameterType);
end;

function TcaRequest.AddParam(AParam: TcaParameter): IcaRequest;
var
  LFile: TcaFileToSend;
begin
  if AParam.Value.IsType<TcaFileToSend> then
  begin
    LFile := AParam.Value.AsType<TcaFileToSend>;
    if not LFile.IsEmpty then
    begin
      LFile.Name := AParam.Name;
      AddFile(LFile, AParam.ParameterType);
    end;
  end
  else if AParam.IsDefaultParameter and AParam.IsRequired then
  begin
    TcaExceptionManager.Current.Alert(ECloudApiRequairedParameterException.Create(FResource, AParam));
  end
  else if AParam.IsDefaultParameter then
  begin
    Exit;
  end
  else
    case AParam.ParameterType of
      TcaParameterType.Cookie:
        FCookies.Add(AParam);
      TcaParameterType.HttpHeader:
        FHttpHeaders.Add(AParam);
      TcaParameterType.GetOrPost:
        FGetOrPosts.Add(AParam);
      TcaParameterType.UrlSegment:
        FUrlSegments.Add(AParam);
      TcaParameterType.RequestBody:
        begin
          if FRequestBody.Text.IsEmpty then
            FRequestBody.Text := AParam.ValueAsString;
        end;
      TcaParameterType.QueryString, TcaParameterType.QueryStringWithoutEncode:
        FQueryStrings.Add(AParam);
    end;
  Result := Self;
end;

function TcaRequest.AddParam(const AName: string; AValue: TValue; AType: TcaParameterType): IcaRequest;
begin
  Result := AddParam(TcaParameter.Create(AName, AValue, TValue.Empty, AType, False));
end;

function TcaRequest.AddQueryParameterJoined(const ANameValue: string; ADelimeter: Char = '='): IcaRequest;
var
  lNameValue: TArray<string>;
begin
  lNameValue := ANameValue.Split([ADelimeter]);
  if Length(lNameValue) <> 2 then
    raise EArgumentException.Create('Cant split ANameValue');
  Result := AddParam(lNameValue[0], lNameValue[1], TcaParameterType.QueryString);
end;

function TcaRequest.AddQueryParameter(const AName, AValue: string): IcaRequest;
begin
  Result := AddParam(AName, AValue, TcaParameterType.QueryString);
end;

function TcaRequest.AddParam(const AName: string; AValue: TValue): IcaRequest;
begin
  Result := AddParam(AName, AValue, TcaParameterType.GetOrPost);
end;

function TcaRequest.AddUrlSegment(const AName, AValue: string): IcaRequest;
begin
  Result := AddParam(AName, AValue, TcaParameterType.UrlSegment);
end;

constructor TcaRequest.Create(const AResource: TUri);
begin
  Self.Create(AResource.ToString, FMethod);
end;

constructor TcaRequest.Create(const AResource: TUri; const AMethod: TcaMethod);
begin
  Self.Create(AResource.ToString, AMethod);
end;

constructor TcaRequest.Create(const AResource: string; const AMethod: TcaMethod);
begin
  Self.Create;
  FResource := AResource;
  FMethod := AMethod;
  { TODO -oMaxim SYSOEV -cGeneral : Parsing AResource - extract queryParameters }
end;

constructor TcaRequest.Create(const AResource: string);
begin
  Self.Create(AResource, FMethod);
end;

procedure TcaRequest.AddFile(const AFile: TcaFileToSend; AParameterType: TcaParameterType);
var
  LParam: TcaParameter;
begin
  case AFile.&Type of
    TcaFileToSendType.&File, TcaFileToSendType.Stream:
      begin
        FFiles.Add(AFile);
      end;
    TcaFileToSendType.URL:
      begin
        LParam := TcaParameter.Create(AFile.Name, AFile.URL, '', AParameterType, True);
        AddParam(LParam);
      end;
    TcaFileToSendType.ID:
      begin
        LParam := TcaParameter.Create(AFile.Name, AFile.ID, '', AParameterType, True);
        AddParam(LParam);
      end;
  else
    raise ENotImplemented.Create('[procedure TcaRequest.AddFile] Report me, if I rise');
  end;

end;

function TcaRequest.AddHeader(const AName, AValue: string): IcaRequest;
begin
  Result := AddParam(AName, AValue, TcaParameterType.HttpHeader);
end;

function TcaRequest.AddHeaders(AHeaders: TArray < TPair < string, string >> ): IcaRequest;
var
  LHeader: TPair<string, string>;
begin
  for LHeader in AHeaders do
  begin
    AddHeader(LHeader.Key, LHeader.Value);
  end;
end;

function TcaRequest.AddQueryParameter(const AName, AValue: string; const AEncode: Boolean): IcaRequest;
var
  LParameterType: TcaParameterType;
begin
  if AEncode then
    LParameterType := TcaParameterType.QueryString
  else
    LParameterType := TcaParameterType.QueryStringWithoutEncode;
  Result := AddParam(AName, AValue, LParameterType);
end;

function TcaRequest.AddQueryParametersJoined(const ANameValues: string; ALineDelimeter, ADelimeter: Char): IcaRequest;
var
  lNameValues: TArray<string>;
  lNameValue: string;
begin
  lNameValues := ANameValues.Split([ALineDelimeter]);
  for lNameValue in lNameValues do
    AddQueryParameterJoined(lNameValue, ADelimeter);
end;

function TcaRequest.GetAlwaysMultipartFormData: Boolean;
begin
  Result := FAlwaysMultipartFormData;
end;

function TcaRequest.GetCookies: TcaParameterList;
begin
  Result := FCookies;
end;

function TcaRequest.GetDefaultParameterType: TcaParameterType;
begin
  Result := FDefaultParameterType;
end;

function TcaRequest.GetFiles: TcaFileList;
begin
  Result := FFiles;
end;

function TcaRequest.GetGetOrPosts: TcaParameterList;
begin
  Result := FGetOrPosts;
end;

function TcaRequest.GetHttpHeaders: TcaParameterList;
begin
  Result := FHttpHeaders;
end;

function TcaRequest.GetLimitInfo: TcaRequestLimit;
begin
  Result := FLimitInfo;
end;

function TcaRequest.GetMethod: TcaMethod;
begin
  Result := FMethod;
end;

function TcaRequest.GetQueryString: TcaParameterList;
begin
  Result := FQueryStrings;
end;

function TcaRequest.GetRequestBody: TStringList;
begin
  Result := FRequestBody;
end;

function TcaRequest.GetResource: string;
begin
  Result := FResource;
end;

function TcaRequest.GetStartAt: TDateTime;
begin
  Result := FStartAt;
end;

function TcaRequest.GetUrlSegments: TcaParameterList;
begin
  Result := FUrlSegments;
end;

function TcaRequest.IsMultipartFormData: Boolean;
begin
  Result := AlwaysMultipartFormData or (FFiles.Count > 0);
end;

function TcaRequest.IsRequestBody: Boolean;
begin
  Result := FRequestBody.Count > 0;
end;

procedure TcaRequest.SetAlwaysMultipartFormData(const Value: Boolean);
begin
  FAlwaysMultipartFormData := Value;
end;

procedure TcaRequest.SetDefaultParameterType(const Value: TcaParameterType);
begin
  FDefaultParameterType := Value;
end;

procedure TcaRequest.SetLimitInfo(const Value: TcaRequestLimit);
begin
  FLimitInfo := Value;
end;

procedure TcaRequest.SetMethod(const Value: TcaMethod);
begin
  FMethod := Value;
end;

procedure TcaRequest.SetResource(const Value: string);
begin
  FResource := Value;
end;

procedure TcaRequest.SetStartAt(const Value: TDateTime);
begin
  FStartAt := Value;
end;

end.
