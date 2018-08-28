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

unit InvisionCommunity.System.Types;

interface

uses
  CloudAPI.Utils.Json;

type
  IicSystemResult = interface
    ['{C753710A-E9F4-4095-9E70-042712E60587}']
    function communityName: string;
    function communityUrl: string;
    function ipsVersion: string;
  end;

  TicSystemResult = class(TBaseJson, IicSystemResult)
    function communityName: string;
    function communityUrl: string;
    function ipsVersion: string;
  end;

implementation

{ TicSystemResult }

function TicSystemResult.communityName: string;
begin
  Result := ToSimpleType<string>('communityName');
end;

function TicSystemResult.communityUrl: string;
begin
  Result := ToSimpleType<string>('communityUrl');
end;

function TicSystemResult.ipsVersion: string;
begin
  Result := ToSimpleType<string>('ipsVersion');
end;

end.

