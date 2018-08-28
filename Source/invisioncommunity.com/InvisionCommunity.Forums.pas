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

unit InvisionCommunity.Forums;

interface

uses
  InvisionCommunity.Base,
  InvisionCommunity.Types,
  InvisionCommunity.Forums.Types;

type
  TicForums = class(TInvCommBase)
  public
    function GetForums: IicArray<IicForumObject>;
    function GetTopics(const Forums, authors: string; //
      const hasBestAnswer, hasPoll, locked, hidden, pinned, featured, archived: Integer; //
      const sortBy, sortDir: string; //
      const page: Integer //
      ): IicArray<IicTopicObject>;
    function GetTopic(const ID: Integer): IicTopicObject;
  end;

implementation

uses
  CloudApi.Utils.Json,
  System.SysUtils,
  System.Net.URLClient;

{ TicSystem }

function TicForums.GetForums: IicArray<IicForumObject>;
begin
  with GetRequest do
  begin
    SetMethod('/api/forums/forums');
    Result := TicArray<IicForumObject>.Create(ExecuteAsString, TicForumObject);
  end;
end;

function TicForums.GetTopic(const ID: Integer): IicTopicObject;
begin
  with GetRequest do
  begin
    SetMethod('/api/forums/topics/' + ID.ToString);
    Result := TBaseJson.AsClass<TicTopicObject>(ExecuteAsString);
    // Result := TicArray<IicTopicObject>.Create(ExecuteAsString, TicTopicObject);
  end;
end;

function TicForums.GetTopics( //
  const Forums, authors: string; //
  const hasBestAnswer, hasPoll, locked, hidden, pinned, featured, archived: Integer; //
  const sortBy, sortDir: string; //
  const page: Integer): IicArray<IicTopicObject>;
begin
  with GetRequest.SetMethod('/api/forums/topics') do
  begin
    AddParameter('forums', Forums, '', False);
    AddParameter('authors', authors, '', False);
    AddParameter('hasBestAnswer', hasBestAnswer, -1, False);
    AddParameter('hasPoll', hasPoll, -1, False);
    AddParameter('locked', locked, -1, False);
    AddParameter('hidden', hidden, -1, False);
    AddParameter('pinned', pinned, -1, False);
    AddParameter('featured', featured, -1, False);
    AddParameter('archived', archived, -1, False);
    AddParameter('sortBy', sortBy, '', False);
    AddParameter('sortDir', sortDir, '', False);
    AddParameter('page', page, -1, False);
    Result := TicArray<IicTopicObject>.Create(ExecuteAsString, TicTopicObject);
  end;
end;

end.
