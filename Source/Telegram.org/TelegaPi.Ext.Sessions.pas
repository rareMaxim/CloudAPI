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

unit TelegaPi.Ext.Sessions;

interface

uses
  System.Rtti,
  System.Generics.Collections;

type
  ItgSession = interface
    ['{D581A266-7AC0-496A-8784-9DCEDC6849C9}']
    function GetItem(const AKey: string): TValue;
    procedure SetItem(const AKey: string; const Value: TValue);
    function GetCreatedAt: TDateTime;
    procedure SetCreatedAt(const Value: TDateTime);
    //
    procedure Clear;
    property Items[const AKey: string]: TValue read GetItem write SetItem; Default;
    property CreatedAt: TDateTime read GetCreatedAt write SetCreatedAt;
  end;

  TtgSession = class(TInterfacedObject, ItgSession)
  private
    FItems: TDictionary<string, TValue>;
    FCreatedAt: TDateTime;
    function GetItem(const AKey: string): TValue;
    procedure SetItem(const AKey: string; const Value: TValue);
    function GetCreatedAt: TDateTime;
    procedure SetCreatedAt(const Value: TDateTime);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property CreatedAt: TDateTime read GetCreatedAt write SetCreatedAt;
    property Items[const AKey: string]: TValue read GetItem write SetItem; Default;
  end;

  ItgSessionManager = interface
    ['{52E3CD5C-C096-4C3D-BC70-D284233C0250}']
    function GetItem(const AID: Int64): ItgSession;
    procedure SetItem(const AID: Int64; const Value: ItgSession);
    procedure Clear;
    property Items[const AID: Int64]: ItgSession read GetItem write SetItem; Default;
  end;

  TtgSessionManager = class(TInterfacedObject, ItgSessionManager)
  private
    class var
      FInstance: TtgSessionManager;
  private
    FItems: TDictionary<Int64, ItgSession>;
    function GetItem(const AID: Int64): ItgSession;
    procedure SetItem(const AID: Int64; const Value: ItgSession);
  public
    class function NewInstance: TObject; override;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Items[const AID: Int64]: ItgSession read GetItem write SetItem; Default;
  end;

implementation

uses
  System.SysUtils;

procedure TtgSession.Clear;
begin
  FItems.Clear;
end;

constructor TtgSession.Create;
begin
  FItems := TDictionary<string, TValue>.Create;
end;

destructor TtgSession.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TtgSession.GetCreatedAt: TDateTime;
begin
  Result := FCreatedAt;
end;

function TtgSession.GetItem(const AKey: string): TValue;
begin
  if not FItems.ContainsKey(AKey) then
    FItems.Add(AKey, TValue.Empty);
  Result := FItems.Items[AKey]
end;

procedure TtgSession.SetCreatedAt(const Value: TDateTime);
begin
  FCreatedAt := Value;
end;

procedure TtgSession.SetItem(const AKey: string; const Value: TValue);
begin
  FItems.AddOrSetValue(AKey, Value);
end;

{ TtgSesionManager }

procedure TtgSessionManager.Clear;
begin
  FItems.Clear;
end;

constructor TtgSessionManager.Create;
begin
  FItems := TDictionary<Int64, ItgSession>.Create;
end;

destructor TtgSessionManager.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TtgSessionManager.GetItem(const AID: Int64): ItgSession;
begin
  if not FItems.ContainsKey(AID) then
    FItems.Add(AID, TtgSession.Create);
  Result := FItems.Items[AID]
end;

class function TtgSessionManager.NewInstance: TObject;
begin
  if FInstance = nil then
    FInstance := TtgSessionManager(inherited NewInstance);
  Result := FInstance;
end;

procedure TtgSessionManager.SetItem(const AID: Int64; const Value: ItgSession);
begin
  FItems.AddOrSetValue(AID, Value);
  FItems.Items[AID].CreatedAt := Now;
end;

end.

