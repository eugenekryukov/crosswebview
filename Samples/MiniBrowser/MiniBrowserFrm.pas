unit MiniBrowserFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CVcl.WebView, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TForm51 = class(TForm)
    Panel1: TPanel;
    URLEdit: TEdit;
    Go: TButton;
    BackButton: TButton;
    ForwButton: TButton;
    WebView1: TWebView;
    StatusBar1: TStatusBar;
    procedure GoClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure ForwButtonClick(Sender: TObject);
    procedure WebView1FinishLoading(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form51: TForm51;

implementation

{$R *.dfm}

procedure TForm51.BackButtonClick(Sender: TObject);
begin
  WebView1.GoBack;
end;

procedure TForm51.ForwButtonClick(Sender: TObject);
begin
  WebView1.GoForward;
end;

procedure TForm51.GoClick(Sender: TObject);
begin
  WebView1.Navigate(URLEdit.Text);
end;

procedure TForm51.WebView1FinishLoading(Sender: TObject);
begin
  StatusBar1.SimpleText := 'Finish loading';
end;

end.
