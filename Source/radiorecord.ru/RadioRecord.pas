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

unit RadioRecord;

interface

uses
  CloudAPI.BaseComponent,
  RadioRecord.Types;

type
  TRadioRecord = class(TCloudApiBaseComponent)
  protected
    procedure DoInitApiCore; override;
  public
    function GetStations: TArray<IrrStation>;
  end;

implementation

uses
  CloudAPI.Utils.Json,
  System.SysUtils,
  System.JSON;
{ TRadioRecord }

procedure TRadioRecord.DoInitApiCore;
begin
  inherited;
  Domain := 'http://www.radiorecord.ru/radioapi/';
  GetRequest.OnDataReceiveAsString :=
    function(AInput: string): string
    var
      LJSON: TJSONObject;
    begin
      if Assigned(OnReceiveRawData) then
        OnReceiveRawData(Self, AInput);
      Result := '';
      if AInput.IsEmpty or AInput.StartsWith('<html') then
        Exit;
      LJSON := TJSONObject.ParseJSONValue(AInput) as TJSONObject;
      try
        Result := LJSON.GetValue('result').ToString;
      finally
        LJSON.Free;
      end;
    end;
  GetRequest.OnDataSend :=
    procedure(AUrl, AData, AHeaders: string)
    begin
      if Assigned(OnSendData) then
        OnSendData(Self, AUrl, AData);
    end;
end;

function TRadioRecord.GetStations: TArray<IrrStation>;
begin
  Result := TBaseJson.AsArray<IrrStation>(TrrStation, GetRequest.SetMethod('stations') //
    .ExecuteAsString);
end;

end.

