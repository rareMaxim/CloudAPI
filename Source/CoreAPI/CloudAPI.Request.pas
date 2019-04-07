{***************************************************************************}
{                                                                           }
{           CloudApi for Delphi                                             }
{                                                                           }
{           Copyright (c) 2014-2018 Maxim Sysoev                            }
{                                                                           }
{           https://t.me/CloudAPI                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit CloudAPI.Request;

interface

uses
  CloudAPI.Utils.Json,
  CloudAPI.Exception,
  CloudAPI.Types,
  System.TypInfo,
  System.Net.Mime,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.Generics.Collections,
  System.SysUtils,
  System.Classes;

type
{$SCOPEDENUMS ON}
  TStoreFormat = (InFormData, InStringList, InUrl, InHeader, Auto);
{$SCOPEDENUMS OFF}

  IApiRequest = interface(IRequestData)
    ['{AF1F11B6-28DB-49EF-AD29-04634D35B15F}']
    // private
    function GetMethodUrl: string;
    procedure SetMethodUrl(const AValue: string);
    function GetOnError: TProc<ECloudApiException>;
    procedure SetOnError(const AValue: TProc<ECloudApiException>);
    function GetHttpClient: THTTPClient;
    procedure SetHttpClient(const AHttpClient: THTTPClient);
    function GetOnDataSend: TProc<string, string, string>;
    procedure SetOnDataSend(const Value: TProc<string, string, string>);
    function GetOnDataReceiveAsString: TFunc<string, string>;
    procedure SetOnDataReceiveAsString(const Value: TFunc<string, string>);
    function GetStoreAutoFormat: TStoreFormat;
    procedure SetStoreAutoFormat(const Value: TStoreFormat);
    function GetOnStaticFill: TProc;
    procedure SetOnStaticFill(const Value: TProc);
    // public
{$REGION 'Add Parameter'}
    function AddParameter(const AKey: string; const AValue, ADefaultValue: string; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue: Int64; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue: TObject; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue: Double; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue: Boolean; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; AValue, ADefaultValue: TFileToSend; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat): IApiRequest; overload;
{$ENDREGION}
    function ClearParams: IApiRequest;
    function Execute: IHTTPResponse;
    function ExecuteAsString: string;
    function ExecuteAsBytes: TArray<Byte>;
    function ExecuteAndReadValue: string;
    function ExecuteAsBool: Boolean;
    function SetMethod(const AValue: string): IApiRequest;
    property MethodUrl: string read GetMethodUrl write SetMethodUrl;
    property HttpClient: THTTPClient read GetHttpClient write SetHttpClient;
    property StoreAutoFormat: TStoreFormat read GetStoreAutoFormat write SetStoreAutoFormat;
    property OnError: TProc<ECloudApiException> read GetOnError write SetOnError;
    property OnDataSend: TProc<string, string, string> read GetOnDataSend write SetOnDataSend;
    property OnDataReceiveAsString: TFunc<string, string> read GetOnDataReceiveAsString write SetOnDataReceiveAsString;
    property OnStaticFill: TProc read GetOnStaticFill write SetOnStaticFill;
  end;

  TApiRequest = class(TRequestData, IApiRequest)
  strict private
    FMethod: string;
    FHttpClient: THTTPClient;
    FOnError: TProc<ECloudApiException>;
    FOnDataSend: TProc<string, string, string>;
    FOnDataReceiveAsString: TFunc<string, string>;
    FStoreAutoFormat: TStoreFormat;
    FOnStaticFill: TProc;
  private
    function GetMethodUrl: string;
    procedure SetMethodUrl(const AValue: string);
    function GetOnError: TProc<ECloudApiException>;
    procedure SetOnError(const AValue: TProc<ECloudApiException>);
    function GetHttpClient: THTTPClient;
    procedure SetHttpClient(const AHttpClient: THTTPClient);
    function GetOnDataSend: TProc<string, string, string>;
    procedure SetOnDataSend(const Value: TProc<string, string, string>);
    function GetOnDataReceiveAsString: TFunc<string, string>;
    procedure SetOnDataReceiveAsString(const Value: TFunc<string, string>);
    function GetStoreAutoFormat: TStoreFormat;
    procedure SetStoreAutoFormat(const Value: TStoreFormat);
    function GetOnStaticFill: TProc;
    procedure SetOnStaticFill(const Value: TProc);
  protected
{$IFDEF TESTINSIGHT}
  public
{$ENDIF}
    procedure DoCheckExecute(AResult: IHTTPResponse);
    function DoExecute_StoreInStringList(const AUrl: string; var AResult: IHTTPResponse): Boolean;
    function DoExecute_StoreInFormData(const AUrl: string; var AResult: IHTTPResponse): Boolean;
    function DoExecute_Get(const AUrl: string; var AResult: IHTTPResponse): Boolean;
    procedure DoStaticFill;
    procedure DoHaveException(E: ECloudApiException; CanBeFree: Boolean = False);
    function NeedAdd(const AValue, ADefaultValue: string; const ARequired: Boolean): Boolean;
    function RaiseArgument(const AValue, ADefaultValue: string; const ARequired: Boolean): Boolean;
    procedure DoStoreParam(const AKey: string; const AValue, ADefaultValue: string; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto);
  public
{$REGION 'Add Parameter'}
    function AddParameter(const AKey: string; const AValue, ADefaultValue: Double; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue: Boolean; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue: string; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue: Int64; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue: TObject; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; AValue, ADefaultValue: TFileToSend; const ARequired:
      Boolean; const AStoreFormat: TStoreFormat): IApiRequest; overload;
{$ENDREGION}
    function ClearParams: IApiRequest; reintroduce;
    function Execute: IHTTPResponse;
    function TryExecute(var AHttpResponse: IHTTPResponse): Boolean;
    function ExecuteAsString: string;
    function ExecuteAsBytes: TArray<Byte>;
    function ExecuteAndReadValue: string;
    function ExecuteAsBool: Boolean;
    function SetMethod(const AValue: string): IApiRequest;
    destructor Destroy; override;
    constructor Create; override;
    property MethodUrl: string read GetMethodUrl write SetMethodUrl;
    property StoreAutoFormat: TStoreFormat read GetStoreAutoFormat write SetStoreAutoFormat;
    property OnError: TProc<ECloudApiException> read GetOnError write SetOnError;
    { Url, Body, Header }
    property OnDataSend: TProc<string, string, string> read GetOnDataSend write SetOnDataSend;
    property OnDataReceiveAsString: TFunc<string, string> read GetOnDataReceiveAsString write SetOnDataReceiveAsString;
    property OnStaticFill: TProc read GetOnStaticFill write SetOnStaticFill;
  end;

  TMultipartFormDataHelper = class helper for TMultipartFormData
    /// <summary>
    /// Add a form data Stream
    /// </summary>
    /// <param name="AFieldName">
    /// Field Name
    /// </param>
    /// <param name="Data">
    /// Stream
    /// </param>
    /// <param name="AFileName">
    /// file name: "File.ext"
    /// </param>
    procedure AddStream(const AFieldName: string; Data: TStream; const AFileName: string);
  end;

implementation

uses
  System.IOUtils;

const
  ERR_CANT_SETUP_STORE_AUTO = 'Нельзя использовать это значение, попробуйте другое';
  ERR_SOME_VALUE = 'В методе "%s": аргумент "%s" имеет значение "%s", которое не может быть таким, как и значение по-умолчанию: "%s"';

{ TApiRequest }

function TApiRequest.RaiseArgument(const AValue, ADefaultValue: string; const ARequired: Boolean): Boolean;
begin
  Result := ARequired and AValue.Equals(ADefaultValue);
end;

function TApiRequest.AddParameter(const AKey: string; const AValue, ADefaultValue: Int64; const
  ARequired: Boolean; const AStoreFormat: TStoreFormat): IApiRequest;
begin
  Result := AddParameter(AKey, AValue.ToString, ADefaultValue.ToString, ARequired, AStoreFormat);
end;

function TApiRequest.AddParameter(const AKey: string; const AValue, ADefaultValue: TObject; const
  ARequired: Boolean; const AStoreFormat: TStoreFormat): IApiRequest;
begin
  Result := AddParameter(AKey, TJsonUtils.ObjectToJString(AValue), //
    TJsonUtils.ObjectToJString(ADefaultValue), ARequired, AStoreFormat);
end;

function TApiRequest.AddParameter(const AKey: string; const AValue, ADefaultValue, ARequired:
  Boolean; const AStoreFormat: TStoreFormat): IApiRequest;
begin
  Result := AddParameter(AKey, AValue.ToString(TUseBoolStrs.True), ADefaultValue.ToString(TUseBoolStrs.True),
    ARequired, AStoreFormat);
end;

function TApiRequest.AddParameter(const AKey: string; const AValue, ADefaultValue: Double; const
  ARequired: Boolean; const AStoreFormat: TStoreFormat): IApiRequest;
const
  CFORMAT = '##.########';
begin
  Result := AddParameter(AKey, //
    FormatFloat(CFORMAT, AValue, TFormatSettings.Invariant), //
    FormatFloat(CFORMAT, ADefaultValue, TFormatSettings.Invariant), //
    ARequired, AStoreFormat);
end;

function TApiRequest.ClearParams: IApiRequest;
begin
  inherited ClearParams;
  DoStaticFill;
  Result := Self;
end;

constructor TApiRequest.Create;
begin
  inherited Create;
  FHttpClient := THTTPClient.Create;
  FHttpClient.AllowCookies := True;
  FStoreAutoFormat := TStoreFormat.InFormData;
  DoStaticFill;
end;

destructor TApiRequest.Destroy;
begin
  FreeAndNil(FHttpClient);
  inherited;
end;

procedure TApiRequest.DoHaveException(E: ECloudApiException; CanBeFree: Boolean);
begin
  if not Assigned(E) then
    Exit;
  if Assigned(OnError) then
    OnError(E)
  else
    raise E;
  if CanBeFree then
    FreeAndNil(E);
end;

procedure TApiRequest.DoStaticFill;
begin
  if Assigned(OnStaticFill) then
    OnStaticFill();
end;

procedure TApiRequest.DoStoreParam(const AKey, AValue, ADefaultValue: string; const ARequired:
  Boolean; const AStoreFormat: TStoreFormat);
begin
  if RaiseArgument(AValue, ADefaultValue, ARequired) then
    raise EArgumentException.CreateFmt(ERR_SOME_VALUE, [FMethod, AKey, AValue, ADefaultValue]);
  if not NeedAdd(AValue, ADefaultValue, ARequired) then
    Exit;
  case AStoreFormat of
    TStoreFormat.InFormData:
      StoreMultipartForm.AddField(AKey, AValue);
    TStoreFormat.InStringList:
      StoreStringList.AddPair(AKey, AValue);
    TStoreFormat.InUrl:
      StoreUrl.AddPair(AKey, AValue);
    TStoreFormat.InHeader:
      StoreHeaders.Add(TNetHeader.Create(AKey, AValue));
    TStoreFormat.Auto:
      DoStoreParam(AKey, AValue, ADefaultValue, ARequired, GetStoreAutoFormat);
  else
    raise ENotImplemented.Create('Unknown StoreFormat');
  end;
end;

procedure TApiRequest.DoCheckExecute(AResult: IHTTPResponse);
begin
  if Assigned(AResult) and (AResult.StatusCode <> 200) then
    DoHaveException(ECloudApiException.Create(AResult.StatusCode, AResult.StatusText, Self), True);
end;

function TApiRequest.DoExecute_Get(const AUrl: string; var AResult: IHTTPResponse): Boolean;
begin
  if Assigned(OnDataSend) then
    OnDataSend(AUrl, '', HeadersToString(StoreHeaders.ToArray));
  AResult := FHttpClient.Get(AUrl, nil, StoreHeaders.ToArray);
  Result := True;
end;

function TApiRequest.DoExecute_StoreInFormData(const AUrl: string; var AResult: IHTTPResponse): Boolean;
begin
  if StoreMultipartForm.Stream.Size > 44 { wtf } then
  begin
    if Assigned(OnDataSend) then
      OnDataSend(AUrl, FormDataToString(StoreMultipartForm), HeadersToString(StoreHeaders.ToArray));
    AResult := FHttpClient.Post(AUrl, StoreMultipartForm, nil, StoreHeaders.ToArray);
    Result := True;
  end
  else
    Result := False;
end;

function TApiRequest.DoExecute_StoreInStringList(const AUrl: string; var AResult: IHTTPResponse): Boolean;
begin
  if StoreStringList.Count > 0 then
  begin
    if Assigned(OnDataSend) then
      OnDataSend(AUrl, StoreStringList.Text, HeadersToString(StoreHeaders.ToArray));
    AResult := FHttpClient.Post(AUrl, StoreStringList, nil, nil, StoreHeaders.ToArray);
    Result := True;
  end
  else
    Result := False;
end;

function TApiRequest.Execute: IHTTPResponse;
var
  LFullUrl: string;
begin
  DoStaticFill;
  Result := nil;
  try
    try

      LFullUrl := string.Join('/', [Domain, MethodUrl]) + '?' + string.Join('&', StoreUrl.ToStringArray);
      if DoExecute_StoreInFormData(LFullUrl, Result) then
        Exit;
      if DoExecute_StoreInStringList(LFullUrl, Result) then
        Exit;
      DoExecute_Get(LFullUrl, Result);
    except
      on E: Exception do
        DoHaveException(ECloudApiException.Create(E), True);
    end;
  finally
    DoCheckExecute(Result);
    ClearParams;
  end;
end;

function TApiRequest.ExecuteAndReadValue: string;
var
  LJson: TBaseJson;
begin
  LJson := TBaseJson.Create(ExecuteAsString);
  try
    Result := LJson.AsString;
  finally
    LJson.Free;
  end;
end;

function TApiRequest.ExecuteAsBool: Boolean;
// var
// LJson: TBaseJson;
begin
  Result := ExecuteAsString = 'true';
  // LJson := TBaseJson.Create(ExecuteAsString);
  // try
  // Result := LJson.AsBoolean;
  // finally
  // LJson.Free;
  // end;
end;

function TApiRequest.ExecuteAsBytes: TArray<Byte>;
var
  LResponse: IHTTPResponse;
begin
  if not TryExecute(LResponse) then
    Exit(nil);
  LResponse.ContentStream.Position := 0;
  SetLength(Result, LResponse.ContentStream.Size);
  LResponse.ContentStream.Read(Result[0], LResponse.ContentStream.Size);
end;

function TApiRequest.ExecuteAsString: string;
var
  LResponse: IHTTPResponse;
begin
  if not TryExecute(LResponse) then
    Exit(string.Empty);
  Result := LResponse.ContentAsString();
  if Assigned(OnDataReceiveAsString) then
    Result := OnDataReceiveAsString(Result);
end;

function TApiRequest.NeedAdd(const AValue, ADefaultValue: string; const ARequired: Boolean): Boolean;
begin
  Result := ARequired or (not AValue.Equals(ADefaultValue));
end;

function TApiRequest.AddParameter(const AKey, AValue, ADefaultValue: string; const ARequired:
  Boolean; const AStoreFormat: TStoreFormat): IApiRequest;
begin
  DoStoreParam(AKey, AValue, ADefaultValue, ARequired, AStoreFormat);
  Result := Self;
end;

function TApiRequest.GetHttpClient: THTTPClient;
begin
  Result := FHttpClient;
end;

function TApiRequest.GetMethodUrl: string;
begin
  Result := FMethod;
end;

function TApiRequest.GetOnDataReceiveAsString: TFunc<string, string>;
begin
  Result := FOnDataReceiveAsString;
end;

function TApiRequest.GetOnDataSend: TProc<string, string, string>;
begin
  Result := FOnDataSend;
end;

function TApiRequest.GetOnError: TProc<ECloudApiException>;
begin
  Result := FOnError;
end;

function TApiRequest.GetOnStaticFill: TProc;
begin
  Result := FOnStaticFill;
end;

function TApiRequest.GetStoreAutoFormat: TStoreFormat;
begin
  Result := FStoreAutoFormat;
end;

procedure TApiRequest.SetHttpClient(const AHttpClient: THTTPClient);
begin
  FHttpClient := AHttpClient;
end;

function TApiRequest.SetMethod(const AValue: string): IApiRequest;
begin
  MethodUrl := AValue;
  Result := Self;
end;

procedure TApiRequest.SetMethodUrl(const AValue: string);
begin
  FMethod := AValue;
end;

procedure TApiRequest.SetOnDataReceiveAsString(const Value: TFunc<string, string>);
begin
  FOnDataReceiveAsString := Value;
end;

procedure TApiRequest.SetOnDataSend(const Value: TProc<string, string, string>);
begin
  FOnDataSend := Value;
end;

procedure TApiRequest.SetOnError(const AValue: TProc<ECloudApiException>);
begin
  FOnError := AValue;
end;

procedure TApiRequest.SetOnStaticFill(const Value: TProc);
begin
  FOnStaticFill := Value;
end;

procedure TApiRequest.SetStoreAutoFormat(const Value: TStoreFormat);
begin
  if Value = TStoreFormat.Auto then
    DoHaveException(ECloudApiException.Create(ERR_CANT_SETUP_STORE_AUTO), True);
  FStoreAutoFormat := Value;
end;

function TApiRequest.TryExecute(var AHttpResponse: IHTTPResponse): Boolean;
begin
  AHttpResponse := Execute;
  Result := Assigned(AHttpResponse);
end;



{ TtgTMultipartFormDataHelper }

procedure TMultipartFormDataHelper.AddStream(const AFieldName: string; Data: TStream; const AFileName: string);
var
  LFileStream: TFileStream;
  LTmpDir: string;
  LTmpFilename: string;
begin
  // get filename for tmp folder e.g. ..\AppData\local\temp\4F353A8AC6AB446D9F592A30B157291B
  LTmpDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + TPath.GetGUIDFileName(False);
  LTmpFilename := IncludeTrailingPathDelimiter(LTmpDir) + ExtractFileName(AFileName);
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

function TApiRequest.AddParameter(const AKey: string; AValue, ADefaultValue: TFileToSend; const
  ARequired: Boolean; const AStoreFormat: TStoreFormat): IApiRequest;
begin
  try
    if ARequired and ((AValue = ADefaultValue) or AValue.IsEmpty) then
    begin
      DoHaveException(ECloudApiException.Create('Not assigned required data'), True);
      Exit;
    end;
    Result := Self;
    case AValue.Tag of
      TFileToSendTag.FromStream:
        StoreMultipartForm.AddStream(AKey, AValue.Content, AValue.Data);
      TFileToSendTag.FromFile:
        StoreMultipartForm.AddFile(AKey, AValue.Data);
      TFileToSendTag.ID, TFileToSendTag.FromURL:
        AddParameter(AKey, AValue.Data, '', ARequired, AStoreFormat);
    else
      DoHaveException(ECloudApiException.Create('Cant convert TTgFileToSend: Unknown prototype tag'), True);
      Exit;
    end;
  finally
    if Assigned(AValue.Content) then
      FreeAndNil(AValue.Content);
  end;
end;

end.

