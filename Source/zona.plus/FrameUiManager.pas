unit FrameUiManager;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  FMX.Forms,
  FMX.Types;

type
  TFrameManager = class
  strict private
    type
      IfmItem = interface
        ['{6890584E-51E5-4D52-881E-FE09002B7324}']
        function SetName(const AName: string): IfmItem;
        function SetFrame(AFrame: TFrame): IfmItem;
        function SetOnShow(OnShow: TProc): IfmItem;
        function SetOnHide(OnHide: TProc): IfmItem;
      end;

      TfmItem = class(TInterfacedObject, IfmItem)
      public
        Name: string;
        Frame: TFrame;
        OnShow, OnHide: TProc;
        function SetName(const AName: string): IfmItem;
        function SetFrame(AFrame: TFrame): IfmItem;
        function SetOnShow(AOnShow: TProc): IfmItem;
        function SetOnHide(AOnHide: TProc): IfmItem;
        constructor Create(const Name: string; Frame: TFrame; OnShow, OnHide: TProc);
        destructor Destroy; override;
      end;

      THistory = class
        FHistory: TList<string>;
      public
        constructor Create;
        procedure Navigate(const APage: string);
        function CanBack: Boolean;
        function Back: string;
        destructor Destroy; override;
      end;
  private
    FParent: TFmxObject;
    FFrames: TObjectList<TfmItem>;
    FHistory: THistory;
    FOnNavigate: TProc<Boolean>;
    procedure SetParent(const Value: TFmxObject);
    procedure Hide(const Name: string);
  public
    function RegFrame<T: TFrame>(const Name: string; OnShow: TProc = nil; OnHide:
      TProc = nil): T; overload;
    procedure Navigate(const Name: string);
    procedure Back;
    function CanBack: Boolean;
    constructor Create(AParent: TFmxObject);
    destructor Destroy; override;
    function getFrame(const Name: string): TFrame; overload;
    function getFrame<T: TFrame>: T; overload;
    function Count: Integer;
  public
    property Parent: TFmxObject read FParent write SetParent;
    property OnNavigate: TProc<Boolean> read FOnNavigate write FOnNavigate;
  end;

implementation

{ TFrameManager }

procedure TFrameManager.Back;
begin
  if CanBack then
  begin                         // 1 - 2 - 3 - 4                            1
    Navigate(FHistory.Back);    // 1 - 2 - 3      --->  1 - 2 - 3 - 3
    FHistory.Back;              // 1 - 2 - 3
  end;
end;

function TFrameManager.CanBack: Boolean;
begin
  Result := FHistory.CanBack;
end;

function TFrameManager.Count: Integer;
begin
  Result := FFrames.Count;
end;

constructor TFrameManager.Create;
begin
  FFrames := TObjectList<TfmItem>.Create;
  FParent := AParent;
  FHistory := THistory.Create;
end;

destructor TFrameManager.Destroy;
begin
  FHistory.Free;
  FFrames.Free;
  inherited;
end;

function TFrameManager.getFrame(const Name: string): TFrame;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FFrames.Count - 1 do
    if FFrames[I].Name = Name then
    begin
      Result := FFrames[I].Frame;
    end;
end;

function TFrameManager.getFrame<T>: T;
var
  I: Integer;
begin
  Result := Nil;
  for I := 0 to FFrames.Count - 1 do
    if FFrames[I].Frame.ClassName = T.ClassName then
    begin
      Result := T(FFrames[I].Frame);
    end;
end;

procedure TFrameManager.Hide(const Name: string);
var
  I: Integer;
begin
  for I := 0 to FFrames.Count - 1 do
  begin
    if FFrames[I].Name = Name then
    begin
      FFrames[I].Frame.Visible := False;
      if Assigned(FFrames[I].OnHide) then
        FFrames[I].OnHide();
    end
  end;
end;

function TFrameManager.RegFrame<T>(const Name: string; OnShow, OnHide: TProc): T;
var
  I: Integer;
begin
  for I := 0 to FFrames.Count - 1 do
    if FFrames[I].Name = Name then
      Exit;

  Result := T.Create(nil);
  Result.Parent := Self.Parent;
  FFrames.Add(TfmItem.Create(Name, Result, OnShow, OnHide));
end;

procedure TFrameManager.SetParent(const Value: TFmxObject);
var
  I: Integer;
begin
  FParent := Value;
  for I := 0 to FFrames.Count - 1 do
    FFrames[I].Frame.Parent := FParent;
end;

procedure TFrameManager.Navigate(const Name: string);
var
  I: Integer;
begin
  for I := 0 to FFrames.Count - 1 do
  begin
    if FFrames[I].Name <> Name then
    begin
      FFrames[I].Frame.Visible := False;
      if Assigned(FFrames[I].OnHide) then
        FFrames[I].OnHide();
    end
    else
    begin
      FFrames[I].Frame.Visible := True;
      if Assigned(FFrames[I].OnShow) then
        FFrames[I].OnShow();
    end;
  end;
  FHistory.Navigate(Name);
  if Assigned(FOnNavigate) then
    FOnNavigate(True);
end;

{ TFrameManager.TfmItem }

constructor TFrameManager.TfmItem.Create(const Name: string; Frame: TFrame;
  OnShow, OnHide: TProc);
begin
  Self.Name := Name;
  Self.Frame := Frame;
  Self.OnShow := OnShow;
  Self.OnHide := OnHide;
  Frame.Visible := False;
  Frame.Align := TAlignLayout.Client;
end;

destructor TFrameManager.TfmItem.Destroy;
begin
  Frame.DisposeOf;
  Frame := nil;
  OnShow := nil;
  OnHide := nil;
  inherited;
end;

function TFrameManager.TfmItem.SetFrame(AFrame: TFrame): IfmItem;
begin
  Frame := AFrame;
  Result := Self;
end;

function TFrameManager.TfmItem.SetName(const AName: string): IfmItem;
begin
  Name := AName;
  Result := Self;
end;

function TFrameManager.TfmItem.SetOnHide(AOnHide: TProc): IfmItem;
begin
  OnHide := AOnHide;
  Result := Self;
end;

function TFrameManager.TfmItem.SetOnShow(AOnShow: TProc): IfmItem;
begin
  OnShow := AOnShow;
  Result := Self;
end;

{ TFrameManager.THistory }

function TFrameManager.THistory.Back: string;
begin
  Result := '';
  if CanBack then
  begin
    FHistory.Delete(FHistory.Count - 1);
    Result := FHistory.Last;
  end;
end;

function TFrameManager.THistory.CanBack: Boolean;
begin
  Result := FHistory.Count > 1;
end;

constructor TFrameManager.THistory.Create;
begin
  FHistory := TList<string>.Create;
end;

destructor TFrameManager.THistory.Destroy;
begin
  FHistory.Free;
  inherited;
end;

procedure TFrameManager.THistory.Navigate(const APage: string);
begin
  if (FHistory.Count = 0) or (FHistory.Last <> APage) then
    FHistory.Add(APage);
end;

end.

