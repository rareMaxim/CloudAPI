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

unit InvisionCommunity.Types;

interface

uses
  CloudAPI.Utils.Json;

type
  IicArray<T> = interface
    ['{A29496B2-1586-4EDA-90A3-5C98B26C7E08}']
    function Page: Integer;
    function PerPage: Integer;
    function TotalResults: Integer;
    function TotalPages: Integer;
    function Results: TArray<T>;
  end;

  TicArray<T: IInterface> = class(TBaseJson, IicArray<T>)
  private
    FBase: TBaseJsonClass;
  public
    function Page: Integer;
    function PerPage: Integer;
    function TotalResults: Integer;
    function TotalPages: Integer;
    function Results: TArray<T>;
    constructor Create(const AJson: string; TgClass: TBaseJsonClass); reintroduce;
  end;

implementation

uses
  InvisionCommunity.Forums.Types;

{ TicArray<T> }

constructor TicArray<T>.Create(const AJson: string; TgClass: TBaseJsonClass);
begin
  inherited Create(AJson);
  FBase := TgClass;
end;

function TicArray<T>.Page: Integer;
begin
  Result := ToSimpleType<Integer>('page');
end;

function TicArray<T>.PerPage: Integer;
begin
  Result := ToSimpleType<Integer>('perPage');
end;

function TicArray<T>.Results: TArray<T>;
begin
  Result := ToArray<T>(FBase, 'results');
end;

function TicArray<T>.TotalPages: Integer;
begin
  Result := ToSimpleType<Integer>('totalPages');
end;

function TicArray<T>.TotalResults: Integer;
begin
  Result := ToSimpleType<Integer>('totalResults');
end;

end.
