unit ZonaClient.UI.ItemView;

interface

uses
  FrameUiManager,
  ZonaAPI.Types,
  ZonaAPI,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FGX.LinkedLabel,
  FMX.TabControl;

type
  TUiItemView = class(TFrame)
    Image1: TImage;
    btn1: TButton;
    vrtscrlbx1: TVertScrollBox;
    grdpnlyt1: TGridPanelLayout;
    lblRaiting: TLabel;
    lblName: TLabel;
    lblYear: TLabel;
    lblGenre: TLabel;
    lblCountry: TLabel;
    Label1: TLabel;
    lbl6: TLabel;
    lblRuntime: TLabel;
    lblReleaseDate: TLabel;
    lblRaitingValue: TLabel;
    lblNameValue: TLabel;
    lblYearValue: TLabel;
    lblGenreValue: TLabel;
    lblCountryValue: TLabel;
    lbl14: TLabel;
    lbl15: TLabel;
    lblRuntimeValue: TLabel;
    lblReleaseDateValue: TLabel;
    txtDescription: TText;
    TabControl1: TTabControl;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    FInfo: IznPageInfo;
    FZone: TZonaAPI;
    procedure DrawHeaderImage(AInfo: IznPageInfo);
    procedure FillHead(AInfo: IznMediaBase);
  public
    { Public declarations }
    procedure FillInfo(AInfo: IznItem);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  FMX.platform,
  System.Threading,
  ZonaAPI.Utils;
{$R *.fmx}

{ TItemView }

procedure TUiItemView.btn1Click(Sender: TObject);
begin
  TTask.Run(
    procedure
    var
      Zona: TZonaAPI;
      FLaunchService: IFGXLaunchService;
    begin
      // Async work
      Zona := TZonaAPI.Create(Self);
      try
        TPlatformServices.Current.SupportsPlatformService(IFGXLaunchService,
          FLaunchService);
        if FLaunchService <> nil then
        begin
          FLaunchService.OpenURL(Zona.GetVideoLink(FInfo).url);
        end;
      finally
        Zona.Free;
      end;
    end);

end;

constructor TUiItemView.Create(AOwner: TComponent);
begin
  inherited;
  FZone := TZonaAPI.Create(Self);
end;

destructor TUiItemView.Destroy;
begin
  FZone.Free;
  inherited;
end;

procedure TUiItemView.DrawHeaderImage(AInfo: IznPageInfo);
begin
  if AInfo = nil then
    Exit;
  if Image1.Width > 1280 then
    TZonaUtils.LoadImgFromUrl(AInfo.backdrops.image_1920, Image1.Bitmap)
  else if Image1.Width > 1000 then
    TZonaUtils.LoadImgFromUrl(AInfo.backdrops.image_1280, Image1.Bitmap)
  else if Image1.Width > 640 then
    TZonaUtils.LoadImgFromUrl(AInfo.backdrops.image_1000, Image1.Bitmap)
  else if Image1.Width > 320 then
    TZonaUtils.LoadImgFromUrl(AInfo.backdrops.image_640, Image1.Bitmap)
  else
    TZonaUtils.LoadImgFromUrl(AInfo.backdrops.image_320, Image1.Bitmap)
end;

procedure TUiItemView.FillHead(AInfo: IznMediaBase);
var
  DesctipionSize: TSizeF;
  MyElem: IznFilterGenres;
begin
  for MyElem in FInfo.genres do
  begin
    lblGenreValue.Text := lblGenreValue.Text + ' ' + MyElem.name;
  end;
  DesctipionSize := TZonaUtils.SizeText(AInfo.description, TPointF.Create(txtDescription.Width,
    99999));
  txtDescription.Height := DesctipionSize.Height;
  txtDescription.Text := AInfo.description;
  lblRaitingValue.Text := AInfo.rating.ToString(TFloatFormat.ffNumber, 1, 1);
  lblNameValue.Text := AInfo.name_rus;
  lblYearValue.Text := AInfo.year.ToString;

  lblCountryValue.Text := AInfo.country;
  lblRuntimeValue.Text := AInfo.runtime.convert;

  lblReleaseDateValue.Text := AInfo.release_date_int;
end;

procedure TUiItemView.FillInfo(AInfo: IznItem);
begin
  TTask.Run(
    procedure
    begin
      // Async work
      FInfo := FZone.GetPageInfo(AInfo);
      DrawHeaderImage(FInfo);
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        var
          i: Integer;
        begin
          // Sync work result
          if FInfo.serial <> nil then
          begin

            FillHead(FInfo.serial);
            TabControl1.Visible := True;
            TabControl1.Clear;
            for i := 1 to FInfo.seasons.count do
              with TabControl1.Add do
              begin
                Text := i.ToString + ' сезон';
              end;
          end
          else
          begin
            FillHead(FInfo.movie);
          end;

        end);

    end);
end;

end.

