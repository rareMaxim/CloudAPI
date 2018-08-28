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

unit InvisionCommunity.Core.Api;

interface

uses
  System.Net.HttpClient,
  System.Net.URLClient,
  System.SysUtils;

type
  IicRequest = interface
    ['{12414A80-A055-4ACC-AA79-1AD6498C2194}']
    function GetUrl: TURI;
    procedure SetUrl(const Value: TURI);
    function GetToken: string;
    procedure SetToken(const Value: string);
    function GetOnError: TProc<Exception>;
    procedure SetOnError(const Value: TProc<Exception>);
    //
    property Token: string read GetToken write SetToken;
    property Url: TURI read GetUrl write SetUrl;
    property OnError: TProc<Exception> read GetOnError write SetOnError;
  end;

  TicRequest = class(TInterfacedObject, IicRequest)
  private
    FToken: string;
    FBasicAuth: TNetHeader;
    FHttp: THTTPClient;
    FUrl: TURI;
    FOnError: TProc<Exception>;
    function GetUrl: TURI;
    procedure SetUrl(const Value: TURI);
    procedure SetToken(const Value: string);
    function GetToken: string;
    function GetOnError: TProc<Exception>;
    procedure SetOnError(const Value: TProc<Exception>);
  protected
    procedure DoCallError(AException: Exception);
    procedure DoCheckError(const AResponse: string);
    procedure SetPath(const APath: string);
    procedure AddParameter(const AKey, AValue: string); overload;
    procedure AddParameter(const AKey: string; const AValue: Integer); overload;

    //
    function Get: string;
  public
    constructor Create; overload;
    constructor Create(const AUrl, AToken: string); overload;
    destructor Destroy; override;
  published
    property Token: string read GetToken write SetToken;
    property Url: TURI read GetUrl write SetUrl;
    property OnError: TProc<Exception> read GetOnError write SetOnError;
  end;

implementation

uses
  System.JSON,
  System.NetEncoding,
  InvisionCommunity.Exceptions;

{ TicRequest }

constructor TicRequest.Create;
begin
  FHttp := THTTPClient.Create;
  FHttp.AllowCookies := True;
end;

procedure TicRequest.AddParameter(const AKey, AValue: string);
begin
  if not AValue.IsEmpty then
    FUrl.AddParameter(AKey, AValue);
end;

procedure TicRequest.AddParameter(const AKey: string; const AValue: Integer);
begin
  if AValue >= 0 then
    AddParameter(AKey, AValue.ToString);
end;

constructor TicRequest.Create(const AUrl, AToken: string);
begin
  Create;
  Url := TURI.Create(AUrl);
  Token := AToken;
end;

destructor TicRequest.Destroy;
begin
  FHttp.Free;
  inherited;
end;

procedure TicRequest.DoCallError(AException: Exception);
begin
  if Assigned(OnError) then
    OnError(AException)
  else
    raise AException;
end;

procedure TicRequest.DoCheckError(const AResponse: string);
var
  FJSON: TJSONObject;
  LTest: string;
  LExcept: TicExcception;
begin
  if AResponse.contains('<!DOCTYPE html>') then
  begin
    LExcept := TicExcception.Create('Website return html');
    try
      DoCallError(LExcept);
      Exit;
    finally
      LExcept.Free;
    end;
  end;

  FJSON := TJSONObject.ParseJSONValue(AResponse) as TJSONObject;
  try
    if FJSON.TryGetValue<string>('errorCode', LTest) then
    begin
      LExcept := TicExcception.Create(FJSON.Values['errorCode'].Value, FJSON.Values
        ['errorMessage'].Value);
      try
        DoCallError(LExcept);
        Exit;
      finally
        LExcept.Free;
      end;
    end;
  finally
    FJSON.Free;
  end
end;

function TicRequest.Get: string;
begin
  try
    Result := FHttp.Get(Url.ToString, nil, [FBasicAuth]).ContentAsString;
  except
    on E: Exception do
      DoCallError(E);
  end;
  DoCheckError(Result);
end;

function TicRequest.GetOnError: TProc<Exception>;
begin
  Result := FOnError
end;

function TicRequest.GetToken: string;
begin
  Result := FToken;
end;

function TicRequest.GetUrl: TURI;
begin
  Result := FUrl;
end;

procedure TicRequest.SetOnError(const Value: TProc<Exception>);
begin
  FOnError := Value;
end;

procedure TicRequest.SetPath(const APath: string);
begin
  FUrl.Path := APath;
end;

procedure TicRequest.SetToken(const Value: string);
begin
  FToken := Value;
  FBasicAuth := TNetHeader.Create('Authorization', 'Basic ' + TNetEncoding.Base64.Encode
    (Value + ':'));
end;

procedure TicRequest.SetUrl(const Value: TURI);
begin
  FUrl := Value;
end;

end.

