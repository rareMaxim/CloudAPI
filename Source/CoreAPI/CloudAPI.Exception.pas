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

unit CloudAPI.Exception;

interface

uses
  System.SysUtils,
  CloudAPI.Types;

type
  ECloudApiException = class(Exception)
  private
    FCode: string;
    FDescription: string;
    FRequest: IRequestData;
    FResponse: string;
    FTime: TDateTime;
  public
    constructor Create(AException: Exception); reintroduce; overload;
    constructor Create(const ACode, ADescription: string; ARequest: IRequestData = nil); reintroduce; overload;
    constructor Create(const ACode: Integer; const ADescription: string; ARequest: IRequestData = nil);
      reintroduce; overload;
    function ToString: string; override;
    property Code: string read FCode write FCode;
    property Description: string read FDescription write FDescription;
    property Request: IRequestData read FRequest write FRequest;
    property Response: string read FResponse write FResponse;
    property Time: TDateTime read FTime write FTime;
  end;

{$IFDEF CONSOLE}
  TOnError = TProc<TObject, ECloudApiException>;
{$ELSE}
  TOnError = procedure(ASender: TObject; const Exception: ECloudApiException) of object;
{$ENDIF}

implementation

{ ECloudApiException }

constructor ECloudApiException.Create(const ACode, ADescription: string; ARequest: IRequestData);
begin
  FCode := ACode;
  FDescription := ADescription;
  FRequest := ARequest;
  FTime := Now;
  inherited Create(FCode + ': ' + Description);
end;

constructor ECloudApiException.Create(const ACode: Integer; const ADescription: string; ARequest: IRequestData);
begin
  Create(ACode.ToString, ADescription, ARequest);
end;

constructor ECloudApiException.Create(AException: Exception);
begin
  Create(0, AException.Message, nil);
end;

function ECloudApiException.ToString: string;
begin
  Result := '[ ' + DateTimeToStr(FTime) + ' ] (' + FCode + ') ' + FDescription;
end;

end.
