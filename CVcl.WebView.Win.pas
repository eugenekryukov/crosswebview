{
  This file is part of CrossVCL WebBroser 
  Copyright (c) 2018 Eugene Kryukov

  The contents of this file are subject to the Mozilla Public License Version 2.0 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
  KIND, either express or implied. See the License for the specific language governing rights and
  limitations under the License.
}

unit CVcl.WebView.Win;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Controls, CVcl.WebView, Winapi.ActiveX, SHDocVw,
  System.Win.IEInterfaces;

type

  IHTMLTxtRange = interface(IDispatch)
    ['{3050F220-98B5-11CF-BB82-00AA00BDCE0B}']
    function Get_htmlText: WideString; safecall;
    procedure Set_text(const p: WideString); safecall;
    function Get_text: WideString; safecall;
  end;

  TWinWebView = class(TInterfacedObject, IWebView)
  private
    FWebBrowser: TWebBrowser;
    FParentWnd: HWnd;
    FDelegate: IWebViewDelegate;
    procedure DocumentComplete(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
    { IWebView }
    procedure WebViewUpdateBounds;
    procedure WebViewNavigate(const URL: string);
    procedure WebViewLoadFromStream(const AStream: TStream; const MimeType, BaseUrl: string);
    procedure WebViewLoadFromString(const AString: string; const BaseUrl: string);
    function WebViewRunScript(const AScript: string; var ResultStr, ErrorStr: string): Boolean;
    function WebViewGetSelectedText: string;
    procedure WebViewGoForward;
    procedure WebViewGoBack;
    function WebGetHandle: Pointer;
  protected
  public
    constructor Create(AParentWnd: HWnd; ADelegate: IWebViewDelegate);
    destructor Destroy; override;
  end;

implementation

{ TWinWebView }

constructor TWinWebView.Create(AParentWnd: HWnd; ADelegate: IWebViewDelegate);
begin
  inherited Create;
  FDelegate := ADelegate;
  FParentWnd := AParentWnd;
  FWebBrowser := TWebBrowser.Create(nil);
  FWebBrowser.ParentWindow := FParentWnd;
  FWebBrowser.OnDocumentComplete := DocumentComplete;
  WebViewUpdateBounds;
end;

destructor TWinWebView.Destroy;
begin
  FWebBrowser.DisposeOf;
  inherited;
end;

procedure TWinWebView.DocumentComplete(ASender: TObject; const pDisp: IDispatch;
  const URL: OleVariant);
begin
  if Assigned(FDelegate) then
    FDelegate.FinishLoading;
end;

function TWinWebView.WebGetHandle: Pointer;
begin
  Result := FWebBrowser;
end;

function TWinWebView.WebViewGetSelectedText: string;
 var
   Document: IHTMLDOcument2;
   SelectionObj: IHTMLSelectionObject;
   SelectionRange: IHtmlTxtRange;
   SelectedText: String;
 begin
   if FWebBrowser.Document <> nil then
   begin
     Document := FWebBrowser.Document as IHTMLDocument2;
     SelectionObj := Document.selection;
     SelectionRange := SelectionObj.CreateRange as IHtmlTxtRange;
     SelectedText := SelectionRange.Get_text;
     Result := SelectedText;
   end
   else
     Result := '';
end;

procedure TWinWebView.WebViewGoBack;
begin
  FWebBrowser.GoForward;
end;

procedure TWinWebView.WebViewGoForward;
begin
  FWebBrowser.GoBack;
end;

procedure TWinWebView.WebViewLoadFromStream(const AStream: TStream; const MimeType, BaseUrl: string);
begin
  if FWebBrowser.Document = nil then
    FWebBrowser.Navigate('about:blank');

  if FWebBrowser.Document <> nil then
    (FWebBrowser.Document as IPersistStreamInit).Load(TStreamAdapter.Create(AStream));
end;

procedure TWinWebView.WebViewLoadFromString(const AString: string; const BaseUrl: string);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(AString);
  try
    WebViewLoadFromStream(StringStream, 'text/html', BaseUrl);
  finally
    StringStream.Free;
  end;
end;

procedure TWinWebView.WebViewNavigate(const URL: string);
begin
  FWebBrowser.Navigate(URL);
end;

function TWinWebView.WebViewRunScript(const AScript: string; var ResultStr, ErrorStr: string): Boolean;
var
  Doc: IHTMLDocument2;
  HTMLWindow: IHTMLWindow2;
  ScriptRes: OleVariant;
begin
  Result := False;

  if FWebBrowser.Document <> nil then
  begin
    Doc := FWebBrowser.Document as IHTMLDocument2;
    if Doc = nil then
      Exit;
    HTMLWindow := Doc.parentWindow;
    if HTMLWindow = nil then
      Exit;
    ScriptRes := HTMLWindow.execScript(AScript, 'JavaScript');

    Result := True;
  end;
end;

procedure TWinWebView.WebViewUpdateBounds;
var
  R: TRect;
begin
  GetClientRect(FParentWnd, R);
  with R do
    FWebBrowser.SetBounds(Left, Top, Width, Height);
end;

end.
