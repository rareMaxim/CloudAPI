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

unit InvisionCommunity;

interface

uses
  CloudApi.Exception,
  System.SysUtils,
  InvisionCommunity.System,
  InvisionCommunity.Forums;

type
  TInvComm = class
  private
    FForums: TicForums;
    FToken: string;
    FUrl: string;
    FSystem: TicSystem;
    FOnError: TOnError;
    procedure SetUrl(const Value: string);
    procedure SetToken(const Value: string);
    procedure SetOnError(const Value: TOnError);
  public
    constructor Create;
    destructor Destroy; override;
    property System: TicSystem read FSystem write FSystem;
    property Forums: TicForums read FForums write FForums;
    property Token: string read FToken write SetToken;
    property Url: string read FUrl write SetUrl;
    property OnError: TOnError read FOnError write SetOnError;
  end;

implementation

{ TInvComm }

constructor TInvComm.Create;
begin
  FSystem := TicSystem.Create(nil);
  FForums := TicForums.Create(nil);
end;

destructor TInvComm.Destroy;
begin
  FSystem.Free;
  FForums.Free;
  inherited;
end;

procedure TInvComm.SetOnError(const Value: TOnError);
begin
  FOnError := Value;
  FSystem.OnError := Value;
  FForums.OnError := Value;
end;

procedure TInvComm.SetToken(const Value: string);
begin
  FToken := Value;
  FSystem.Token := Value;
  FForums.Token := Value;
end;

procedure TInvComm.SetUrl(const Value: string);
begin
  FUrl := Value;
  FSystem.Url := Value;
  FForums.Url := Value;
end;

end.
