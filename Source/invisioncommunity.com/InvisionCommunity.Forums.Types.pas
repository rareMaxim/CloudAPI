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

unit InvisionCommunity.Forums.Types;

interface

uses
  CloudAPI.Utils.Json;

type
{$REGION 'ForumObject'}
  IicForumObject = interface
    ['{9D49119A-BCD0-4DFC-896E-69CD03E49F64}']
    function ID: Integer;
    function Name: string;
    function topics: Integer;
    function url: string;
  end;

  TicForumObject = class(TBaseJson, IicForumObject)
    function ID: Integer;
    function Name: string;
    function topics: Integer;
    function url: string;
  end;
{$ENDREGION}
{$REGION 'GroupObject'}

  IicGroupObject = interface
    ['{08B31C3C-183A-4B83-9C73-9D8492201F10}']
    function ID: Integer;
    function Name: string;
    function formattedName: string;
  end;
{$ENDREGION}
{$REGION 'MemberObject'}

  IicFieldGroupObject = interface
    ['{0F680A72-E40C-4447-B7B9-84F7BD60A4AD}']
    function Name: string;
    function fields: TArray<IicGroupObject>;
  end;
{$ENDREGION}
{$REGION 'MemberObject'}

  IicMemberObject = interface
    ['{143E1882-93EA-400B-96E7-4B3309F6FB5A}']
    function ID: Integer;
    function Name: string;
    function Title: string;
    function timezone: string;
    function formattedName: string;
    function primaryGroup: IicGroupObject;
    function secondaryGroups: TArray<IicGroupObject>;
    function email: string;
    function joined: TDateTime;
    function registrationIpAddress: string;
    function photoUrl: string;
    function profileUrl: string;
    function validating: Boolean;
    function Posts: Integer;
    function lastActivity: TDateTime;
    function lastVisit: TDateTime;
    function lastPost: TDateTime;
    function profileViews: Integer;
    function birthday: string;
    function customFields: TArray<IicFieldGroupObject>;
  end;
{$ENDREGION}
{$REGION 'QuestionObject'}

  IicQuestionObject = interface
    ['{11087C04-8C07-4D43-80F2-BF58570FC09D}']
    function question: string;
    function options: string;
  end;
{$ENDREGION}
{$REGION 'PollObject'}

  IicPollObject = interface
    ['{52C3BADE-2F31-4A67-8F43-5BF8A12CE871}']
    function ID: Integer;
    function Title: string;
    function votes: Integer;
    function questions: TArray<IicQuestionObject>;
  end;
{$ENDREGION}
{$REGION 'PostObject'}

  IicPostObject = interface
    ['{E10485F0-BEA6-47E0-AE04-C655F101E0DD}']
    function ID: Integer;
    function ItemID: Integer;
    function Author: IicMemberObject;
    function date: TDateTime;
    function content: string;
    function hidden: Boolean;
    function url: string;
  end;
{$ENDREGION}
{$REGION 'TopicObject'}

  IicTopicObject = interface
    ['{33162B6C-8BEE-41FD-87CE-0946C08A9C00}']
    function ID: Integer;
    function Title: string;
    function Forum: IicForumObject;
    function Posts: Integer;
    function Views: Integer;
    function Prefix: string;
    function tags: TArray<string>;
    function firstPost: IicPostObject;
    function lastPost: IicPostObject;
    function bestAnswer: IicPostObject;
    function locked: Boolean;
    function hidden: Boolean;
    function pinned: Boolean;
    function featured: Boolean;
    function archived: Boolean;
    function poll: IicPollObject;
    function Url: string;
    function rating: Single;
  end;

  TicTopicObject = class(TBaseJson, IicTopicObject)
    function ID: Integer;
    function Title: string;
    function Forum: IicForumObject;
    function Posts: Integer;
    function Views: Integer;
    function Prefix: string;
    function tags: TArray<string>;
    function firstPost: IicPostObject;
    function lastPost: IicPostObject;
    function bestAnswer: IicPostObject;
    function locked: Boolean;
    function hidden: Boolean;
    function pinned: Boolean;
    function featured: Boolean;
    function archived: Boolean;
    function poll: IicPollObject;
    function url: string;
    function rating: Single;
  end;
{$ENDREGION}

implementation

{ TicTopicObject }

function TicForumObject.ID: Integer;
begin
  Result := ToSimpleType<Integer>('id');
end;

function TicForumObject.Name: string;
begin
  Result := ToSimpleType<string>('name');
end;

function TicForumObject.topics: Integer;
begin
  Result := ToSimpleType<Integer>('topics');
end;

function TicForumObject.url: string;
begin
  Result := ToSimpleType<string>('url');
end;

{ TicTopicObject }

function TicTopicObject.archived: Boolean;
begin
  Result := ToSimpleType<Boolean>('archived');
end;

function TicTopicObject.bestAnswer: IicPostObject;
begin

end;

function TicTopicObject.featured: Boolean;
begin
  Result := ToSimpleType<Boolean>('featured');
end;

function TicTopicObject.firstPost: IicPostObject;
begin

end;

function TicTopicObject.Forum: IicForumObject;
begin

end;

function TicTopicObject.hidden: Boolean;
begin
  Result := ToSimpleType<Boolean>('hidden');
end;

function TicTopicObject.ID: Integer;
begin
  Result := ToSimpleType<Integer>('id');
end;

function TicTopicObject.lastPost: IicPostObject;
begin

end;

function TicTopicObject.locked: Boolean;
begin
  Result := ToSimpleType<Boolean>('locked');
end;

function TicTopicObject.pinned: Boolean;
begin
  Result := ToSimpleType<Boolean>('pinned');
end;

function TicTopicObject.poll: IicPollObject;
begin

end;

function TicTopicObject.Posts: Integer;
begin
  Result := ToSimpleType<Integer>('posts');
end;

function TicTopicObject.Prefix: string;
begin
  Result := ToSimpleType<string>('prefix');
end;

function TicTopicObject.rating: Single;
begin
  Result := ToSimpleType<Single>('rating');
end;

function TicTopicObject.tags: TArray<string>;
begin

end;

function TicTopicObject.Title: string;
begin
  Result := ToSimpleType<string>('title');
end;

function TicTopicObject.url: string;
begin
  Result := ToSimpleType<string>('url');
end;

function TicTopicObject.Views: Integer;
begin
  Result := ToSimpleType<Integer>('views');
end;

end.

