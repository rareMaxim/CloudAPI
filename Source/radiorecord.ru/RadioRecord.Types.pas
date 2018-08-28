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

unit RadioRecord.Types;

interface

uses
  CloudAPI.Utils.Json;

type
  IrrStation = interface
    ['{77E83566-FD93-4691-8DF9-6A038DB49BD1}']
    function prefix: string;
    function title: string;
    function artist: string;
    function song: string;
    function phone: string;
    function icon: string;
    function icon_png: string;
    function has_feedback: Boolean;
    function feedback_text: string;
    function stream: string;
    function stream_32: string;
    function stream_64: string;
    function stream_128: string;
    function stream_320: string;
    function schedule: string;
  end;

  TrrStation = class(TBaseJson, IrrStation)
    function prefix: string;
    function title: string;
    function artist: string;
    function song: string;
    function phone: string;
    function icon: string;
    function icon_png: string;
    function has_feedback: Boolean;
    function feedback_text: string;
    function stream: string;
    function stream_32: string;
    function stream_64: string;
    function stream_128: string;
    function stream_320: string;
    function schedule: string;
  end;

implementation

{ TrrStation }

function TrrStation.artist: string;
begin
  Result := ToSimpleType<string>('artist');
end;

function TrrStation.feedback_text: string;
begin
  Result := ToSimpleType<string>('feedback_text');
end;

function TrrStation.has_feedback: Boolean;
begin
  Result := ToSimpleType<Boolean>('has_feedback');
end;

function TrrStation.icon: string;
begin
  Result := ToSimpleType<string>('icon');
end;

function TrrStation.icon_png: string;
begin
  Result := ToSimpleType<string>('icon_png');
end;

function TrrStation.phone: string;
begin
  Result := ToSimpleType<string>('phone');
end;

function TrrStation.prefix: string;
begin
  Result := ToSimpleType<string>('prefix');
end;

function TrrStation.schedule: string;
begin
  Result := ToSimpleType<string>('schedule');
end;

function TrrStation.song: string;
begin
  Result := ToSimpleType<string>('song');
end;

function TrrStation.stream: string;
begin
  Result := ToSimpleType<string>('stream');
end;

function TrrStation.stream_128: string;
begin
  Result := ToSimpleType<string>('stream_128');
end;

function TrrStation.stream_32: string;
begin
  Result := ToSimpleType<string>('stream_32');
end;

function TrrStation.stream_320: string;
begin
  Result := ToSimpleType<string>('stream_320');
end;

function TrrStation.stream_64: string;
begin
  Result := ToSimpleType<string>('stream_64');

end;

function TrrStation.title: string;
begin
  Result := ToSimpleType<string>('title');

end;

end.

