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

unit CloudAPI.GoogleAnalitics;

interface

uses
  CloudAPI.BaseComponent,
  CloudAPI.GoogleAnalitics.Extensions,
  System.Classes,
  System.SysUtils;

type
  /// <summary>
  /// Класс для работы с Analitics.Google.com: Measurement Protocol.
  /// </summary>
  TgaOnDebug = procedure(Sender: TObject; const AMsg: string) of object;

  TgaAnalitics = class(TCloudApiBaseComponent)
  private
    const
      GA_SERVER = 'https://www.google-analytics.com';
      GA_METHOD_STABLE = 'collect';
      GA_METHOD_DEBUG = 'debug/collect';
  private
    FGeneral: TgaGeneral;
    FUser: TgaUser;
    FSystem: TgaSystem;
    FSessionController: TgaSession;
    FContentInformation: TgaContentInformation;
    FException: TgaException;
    FHit: TgaHit;
    FTrafficSources: TgaTrafficSources;
    FAppTracking: TgaAppTracking;
    FOnDebug: TgaOnDebug;
    FIsDebug: Boolean;
    FDebugMsg: string;
  protected
    procedure DoFillData(const AType: TgaTypeTreatment);
    function Execute(const AUrl: string): TBytes;
    procedure DoExecute;
    procedure DoDebug(const AMessage: string);
  public
    procedure PageView(const APage: string);
    procedure ScreenView(const AScreen: string);
    procedure HaveException(const AText: string; AIsFatal: Boolean = True);

    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
  published
    property General: TgaGeneral read FGeneral write FGeneral;
    property User: TgaUser read FUser write FUser;
    property Session: TgaSession read FSessionController write FSessionController;
    property TrafficSources: TgaTrafficSources read FTrafficSources write FTrafficSources;
    property SystemInfo: TgaSystem read FSystem write FSystem;
    property Hit: TgaHit read FHit write FHit;
    property ContentInformation: TgaContentInformation read FContentInformation
      write FContentInformation;
    property AppTracking: TgaAppTracking read FAppTracking write FAppTracking;
    property Exception: TgaException read FException write FException;
    property OnSendData;
    property OnDebug: TgaOnDebug read FOnDebug write FOnDebug;
    property IsDebug: Boolean read FIsDebug write FIsDebug;
    property DebugMsg: string read FDebugMsg;
  end;
{$SCOPEDENUMS ON}

procedure register;

implementation

uses
  CloudAPI.Request;

procedure register;
begin
  RegisterComponents('CloudAPI', [TgaAnalitics]);
end;

  { TgaAnalitics }
constructor TgaAnalitics.Create(AOwner: TComponent);
begin
  inherited;
  FGeneral := TgaGeneral.Create;
  FUser := TgaUser.Create;
  FSessionController := TgaSession.Create;
  FTrafficSources := TgaTrafficSources.Create;
  FSystem := TgaSystem.Create;
  FHit := TgaHit.Create;
  FContentInformation := TgaContentInformation.Create;
  FAppTracking := TgaAppTracking.Create;
  FException := TgaException.Create;
end;

destructor TgaAnalitics.Destroy;
begin
  FreeAndNil(FGeneral);
  FreeAndNil(FUser);
  FreeAndNil(FSessionController);
  FreeAndNil(FTrafficSources);
  FreeAndNil(FSystem);
  FreeAndNil(FHit);
  FreeAndNil(FContentInformation);
  FreeAndNil(FAppTracking);
  FreeAndNil(FException);
  inherited;
end;

procedure TgaAnalitics.DoDebug(const AMessage: string);
begin
  if Assigned(OnDebug) then
    OnDebug(Self, AMessage);
end;

procedure TgaAnalitics.DoExecute;
begin
  Execute(GA_SERVER);
end;

procedure TgaAnalitics.DoFillData(const AType: TgaTypeTreatment);
begin
  General.FillData(AType, GetRequest.StoreUrl);
  User.FillData(AType, GetRequest.StoreUrl);
  Session.FillData(AType, GetRequest.StoreUrl);
  TrafficSources.FillData(AType, GetRequest.StoreUrl);
  SystemInfo.FillData(AType, GetRequest.StoreUrl);
  Hit.FillData(AType, GetRequest.StoreUrl);
  ContentInformation.FillData(AType, GetRequest.StoreUrl);
  AppTracking.FillData(AType, GetRequest.StoreUrl);
  Exception.FillData(AType, GetRequest.StoreUrl);
end;

procedure TgaAnalitics.HaveException(const AText: string; AIsFatal: Boolean);
begin
  Hit.HitType := TgaTypeTreatment.Exception;
  Exception.Text := AText;
  Exception.IsFatal := AIsFatal;
  DoExecute;
end;

function TgaAnalitics.Execute(const AUrl: string): TBytes;
begin
  DoFillData(Hit.HitType);
  GetRequest.Domain := AUrl;
  if IsDebug then
  begin
    GetRequest.MethodUrl := GA_METHOD_DEBUG;
    Result := GetRequest.ExecuteAsBytes;
    FDebugMsg := TEncoding.Default.GetString(Result);
    DoDebug(FDebugMsg);
  end
  else
  begin
    GetRequest.MethodUrl := GA_METHOD_STABLE;
    Result := GetRequest.ExecuteAsBytes;
  end;
end;

procedure TgaAnalitics.PageView(const APage: string);
begin
  ContentInformation.DocumentPath := APage;
  Hit.HitType := TgaTypeTreatment.screenview;
  DoExecute;
end;

procedure TgaAnalitics.ScreenView(const AScreen: string);
begin
  ContentInformation.ScreenName := AScreen;
  Hit.HitType := TgaTypeTreatment.screenview;
  DoExecute;
end;

end.

