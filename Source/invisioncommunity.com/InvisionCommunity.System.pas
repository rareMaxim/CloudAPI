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

unit InvisionCommunity.System;

interface

uses
  InvisionCommunity.System.Types,
  InvisionCommunity.Base;

type
  TicSystem = class(TInvCommBase)
  private
  public
    function Hello: IicSystemResult;
  end;

implementation

{ TicSystem }

function TicSystem.Hello: IicSystemResult;
begin
  with GetRequest do
  begin
    SetMethod('/api/core/hello');
    Result := TicSystemResult.Create(ExecuteAsString);
  end;
end;

end.
