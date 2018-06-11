unit ZonaAPI.Utils;

interface

uses
  System.Generics.Collections,
  FMX.TabControl,
  FMX.ListView.Appearances,
  FMX.Objects,
  System.Types,
  System.SysUtils,
  FMX.Graphics,
  FMX.Types;

type
  TZonaUtils = class
    class procedure LoadImgFromUrl(const aURL: string; ABitmap: TBitmap);
    class procedure FreeAndNil(Obj: TFmxObject);
    class function SizeText(const AText: string; AMaxSize: TPointF): TSizeF;
  end;

  TMediaItem = class
    URL: string;
    bitmap: TBitmap;
  end;

  TMediaLib = class
    FList: TObjectList<TMediaItem>;
    FStop: Boolean;
  public
    procedure Load(ABitmap: TBitmap; const aURL: string);
    constructor Create;
    procedure Clear;
    procedure Run;
    procedure Stop;
    destructor Destroy; override;
  end;

  TTabControlHelper = class helper for TTabControl
    procedure Clear;
  end;

implementation

uses
  FMX.TextLayout,
  System.Threading,
  System.Classes,
  System.Math,
  System.Net.HttpClient;
{ TZonaUtils }

class function TZonaUtils.SizeText(const AText: string; AMaxSize: TPointF): TSizeF;
var
  Layout: TTextLayout;
begin
  Layout := TTextLayoutManager.DefaultTextLayout.Create(nil);
  try
    Layout.BeginUpdate;
    try
      Layout.MaxSize := AMaxSize;
      Layout.Text := AText;
      Layout.WordWrap := True;
    finally
      Layout.EndUpdate;
    end;
    Result := TSizeF.Create(Layout.Width, Layout.Height);
  finally
    System.SysUtils.FreeAndNil(Layout);
  end;
end;

class procedure TZonaUtils.FreeAndNil(Obj: TFmxObject);
begin
  if Assigned(Obj) then
  begin
    Obj.DisposeOf; // Obj.Release;
    Obj := nil;
  end;
end;

class procedure TZonaUtils.LoadImgFromUrl(const AUrl: string; ABitmap: TBitmap);
var
  LHttp: THTTPClient;
begin

  LHttp := THTTPClient.Create;
  try
   // LoadBitmapFromURL(AUrl, ABitmap);
    if not Assigned(ABitmap) then
      Exit;
    ABitmap.LoadFromStream(LHttp.Get(AUrl).ContentStream);
  finally
    LHttp.Free;
  end;
end;
{ TMediaLib }

procedure TMediaLib.Clear;
begin
  FList.Clear;
end;

constructor TMediaLib.Create;
begin
  FList := TObjectList<TMediaItem>.Create;
  FStop := False;
end;

destructor TMediaLib.Destroy;
begin
 // Clear;
  Stop;
  FreeAndNil(FList);
  inherited;
end;

procedure TMediaLib.Load(ABitmap: TBitmap; const aURL: string);
var
  x: TMediaItem;
begin
  x := TMediaItem.create;
  x.bitmap := ABitmap;
  x.URL := aURL;
  FList.Add(x);
end;

procedure TMediaLib.Run;
var
  LTmp: TBitmap;
begin
  FStop := False;
  TTask.Run(
    procedure
    var
      I: Integer;
    begin
      for I := 0 to FList.Count - 1 do
      begin
        LTmp := TBitmap.Create();
        try
          TZonaUtils.LoadImgFromUrl(FList[I].URL, LTmp);
          if FStop then
            Break;
          TThread.Synchronize(nil,
            procedure
            begin
              FList[I].bitmap.Assign(LTmp);
            end);
        finally
          LTmp.Free;
        end;
      end;
    end);
end;

procedure TMediaLib.Stop;
begin
  FStop := True;
  Clear;
end;

{ TMediaItem }

{ TTabControlHelper }

procedure TTabControlHelper.Clear;
var
  I: Integer;
begin
  for I := TabCount - 1 downto 0 do
    Delete(I);
end;

end.

