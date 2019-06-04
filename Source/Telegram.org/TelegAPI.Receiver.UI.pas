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

unit TelegAPI.Receiver.UI;

interface

uses
  TelegAPI.Receiver.Service,
  TelegAPi.Types;

type
  TtgReceiverUI = class(TtgReceiverService)
  protected
    procedure EventParser(AUpdates: System.TArray<TelegAPI.Types.ItgUpdate>); override;
    procedure DoOnStart; override;
    procedure DoOnStop; override;
  end;

implementation

uses
  System.Classes;
{ TtgRecesiverUI }

procedure TtgReceiverUI.DoOnStart;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      inherited DoOnStart;
    end);
end;

procedure TtgReceiverUI.DoOnStop;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      inherited DoOnStop;
    end);
end;

procedure TtgReceiverUI.EventParser(AUpdates: System.TArray<TelegAPI.Types.ItgUpdate>);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      inherited EventParser(AUpdates);
    end);
end;

end.

