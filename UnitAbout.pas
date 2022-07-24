unit UnitAbout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects;

type
  TFormAbout = class(TForm)
    ImageLogo: TImage;
    LabelAbout: TLabel;
    ButtonOK: TButton;
    LabelVersion: TLabel;
    LabelAuthor: TLabel;
    Label1: TLabel;
    procedure ButtonOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.fmx}

procedure TFormAbout.ButtonOKClick(Sender: TObject);
begin
  Close;
end;

end.
