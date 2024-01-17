$PBExportHeader$w_carga_json.srw
$PBExportComments$Generacion de Archivos XML
forward
global type w_carga_json from w_para_informes
end type
type st_4 from statictext within w_carga_json
end type
type st_1 from statictext within w_carga_json
end type
type uo_selcliente from uo_seleccion_clientesprod within w_carga_json
end type
type st_2 from statictext within w_carga_json
end type
type dw_1 from uo_dw within w_carga_json
end type
type dw_2 from uo_dw within w_carga_json
end type
type mle_texto from multilineedit within w_carga_json
end type
type sle_embalaje from singlelineedit within w_carga_json
end type
type dw_embalaje from uo_dw within w_carga_json
end type
type cb_1 from commandbutton within w_carga_json
end type
end forward

global type w_carga_json from w_para_informes
string tag = "Generacion de Archivos XML"
integer x = 14
integer y = 32
integer width = 2583
integer height = 1696
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "AppIcon!"
boolean clientedge = true
st_4 st_4
st_1 st_1
uo_selcliente uo_selcliente
st_2 st_2
dw_1 dw_1
dw_2 dw_2
mle_texto mle_texto
sle_embalaje sle_embalaje
dw_embalaje dw_embalaje
cb_1 cb_1
end type
global w_carga_json w_carga_json

type variables
Private Constant	String	_URL = 'https://rioblanco-api-packing.azurewebsites.net/'
end variables

forward prototypes
public function boolean wf_graba (boolean borrando)
public function boolean wf_envase (integer tipo, integer envase)
public function boolean wf_embalaje (string embalaje)
public subroutine wf_inserta (string mensaje)
public function boolean wf_tipopallemba (long cliente, string embalaje)
public function boolean wf_gtin (long cliente, string embalaje)
end prototypes

public function boolean wf_graba (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If Borrando Then
	If dw_1.Update(True, False) = 1 Then
		Commit;
		
		If sqlca.SQLCode <> 0 Then
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		Else
			lb_Retorno	=	True
			
			dw_1.ResetUpdate()
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
	End If
Else
	If dw_1.Update(True, False) = 1 Then
		Commit;
		
		If sqlca.SQLCode <> 0 Then
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		Else
			lb_Retorno	=	True
			
			dw_1.ResetUpdate()
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	End If
End If

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

public function boolean wf_envase (integer tipo, integer envase);Boolean 		lb_Retorno = True
RESTClient	lnv_RestClient
String 		ls_URL, ls_Busca
Long			ll_Busca, ll_New, ll_Fila


lnv_RestClient	=	Create RESTClient
lnv_RestClient.SetRequestHeader ("Content-Type", "application/json;charset=UTF-8")

dw_2.DataObject = 'dw_mues_envases'
dw_1.DataObject = 'dw_mues_envases'
dw_1.SetTransObject(SQLCA)
dw_1.Retrieve(Tipo)

ls_URL = _URL + 'api/Envases/RetrieveOne/' + String(Tipo) + '/'  + String(Envase)
lnv_RestClient.RetrieveOne(dw_2, ls_URL)

For ll_Fila = 1 To dw_2.RowCount()
	ls_Busca = 'enva_tipoen = ' + String(Tipo) + ' and enva_codigo = '  + String(Envase)
	ll_Busca = dw_1.Find(ls_Busca, 1, dw_1.RowCount(), Primary!)
	
	If ll_Busca = 0 Then
		dw_2.RowsCopy(ll_Fila, ll_Fila, Primary!, dw_1, 1, Primary!)
		wf_Inserta('Se inserta envase tipo:'  + String(Tipo) + ', Codigo Envase: ' + String(Envase, '00'))
	Else
		dw_1.Object.enva_Nombre[ll_Busca]	= dw_2.Object.enva_Nombre[ll_Fila]
		dw_1.Object.enva_Pesone[ll_Busca]	= dw_2.Object.enva_Pesone[ll_Fila]
		dw_1.Object.enva_Pesobr[ll_Busca]	= dw_2.Object.enva_Pesobr[ll_Fila]
		dw_1.Object.enva_Dfaesp[ll_Busca]	= dw_2.Object.enva_Dfaesp[ll_Fila]
		dw_1.Object.enva_Dfaing[ll_Busca]	= dw_2.Object.enva_Dfaing[ll_Fila]
		dw_1.Object.Emes_Codigo[ll_Busca]	= dw_2.Object.Emes_Codigo[ll_Fila]
//		dw_1.Object.enva_Kilcua[ll_Busca]	= dw_2.Object.enva_Kilcua[ll_Fila]
		dw_1.Object.enva_Noming[ll_Busca]	= dw_2.Object.enva_Noming[ll_Fila]
//		dw_1.Object.enva_Tipoma[ll_Busca]	= dw_2.Object.enva_Tipoma[ll_Fila]
//		dw_1.Object.enva_Unidad[ll_Busca]	= dw_2.Object.enva_Unidad[ll_Fila]
//		dw_1.Object.enva_Agrupa[ll_Busca]	= dw_2.Object.enva_Agrupa[ll_Fila]
		dw_1.Object.enva_Peslib[ll_Busca]	= dw_2.Object.enva_Peslib[ll_Fila]
		dw_1.Object.enva_Pestra[ll_Busca]	= dw_2.Object.enva_Pestra[ll_Fila]
		dw_1.Object.enva_Perten[ll_Busca]	= dw_2.Object.enva_Perten[ll_Fila]
		dw_1.Object.enva_Caifco [ll_Busca]	= dw_2.Object.enva_Caifco[ll_Fila]
		wf_Inserta('Se actualizo envase tipo:'  + String(Tipo) + ', Codigo Envase: ' + String(Envase, '00'))
	End If
Next

If wf_Graba(False) Then wf_Inserta('Se grabo envase tipo:'  + String(Tipo) + ', Codigo Envase: ' + String(Envase, '00'))

Destroy lnv_RestClient

Return lb_Retorno
end function

public function boolean wf_embalaje (string embalaje);Boolean 		lb_Retorno = True
RESTClient	lnv_RestClient
String 		ls_URL, ls_Busca
Long			ll_Busca, ll_New, ll_Fila


wf_Inserta('Proceso de creacion de embalajes...')
wf_Inserta('Recupera embalaje ' + Upper(embalaje))

lnv_RestClient	=	Create RESTClient
lnv_RestClient.SetRequestHeader ("Content-Type", "application/json;charset=UTF-8")

dw_embalaje.DataObject = 'dw_mues_embalajesprod_mantvig'
ls_URL = _URL + 'api/Embalajesprod/RetrieveOne/' + Embalaje + '/' + String(uo_SelCliente.Codigo)
lnv_RestClient.RetrieveOne(dw_embalaje, ls_URL)

If dw_Embalaje.RowCount() > 0 Then

	wf_Envase(dw_Embalaje.Object.enva_tipoen[1], dw_Embalaje.Object.enva_codigo[1])
	
	dw_1.DataObject = 'dw_mues_embalajesprod_mantvig'
	dw_1.SetTransObject(SQLCA)
	dw_1.Retrieve(uo_SelCliente.Codigo, -1, '-1', -1, '-1')
		
	For ll_Fila = 1 To dw_embalaje.RowCount()
		ls_Busca = 'emba_codigo = "'  + Trim(Embalaje)+ '"'
		ll_Busca = dw_1.Find(ls_Busca, 1, dw_1.RowCount())
	
		If ll_Busca = 0 Then
			dw_embalaje.RowsCopy(ll_Fila, ll_Fila, Primary!, dw_1, 1, Primary!)
			wf_Inserta('Se inserta embalaje:'  + embalaje)
		Else
			dw_1.Object.envo_Codigo[ll_Busca] 		= dw_embalaje.Object.envo_Codigo[ll_Fila]
			dw_1.Object.enva_Tipoen[ll_Busca] 		= dw_embalaje.Object.enva_Tipoen[ll_Fila]
			dw_1.Object.enva_Codigo[ll_Busca] 		= dw_embalaje.Object.enva_Codigo[ll_Fila]
			dw_1.Object.emba_Nombre[ll_Busca]	= dw_embalaje.Object.emba_Nombre[ll_Fila]
			dw_1.Object.emba_Abrevi[ll_Busca] 		= dw_embalaje.Object.emba_Abrevi[ll_Fila]
			dw_1.Object.emba_Cajpal[ll_Busca] 		= dw_embalaje.Object.emba_Cajpal[ll_Fila]
			dw_1.Object.emba_Nomlar[ll_Busca] 	= dw_embalaje.Object.emba_Nomlar[ll_Fila]
			dw_1.Object.emes_Codigo[ll_Busca] 		= dw_embalaje.Object.emes_Codigo[ll_Fila]
			dw_1.Object.emba_Tiprpc[ll_Busca] 		= dw_embalaje.Object.emba_Tiprpc[ll_Fila]
			dw_1.Object.emba_Dimens[ll_Busca] 	= dw_embalaje.Object.emba_Dimens[ll_Fila]
			dw_1.Object.espe_Codigo[ll_Busca] 		= dw_embalaje.Object.espe_Codigo[ll_Fila]
			dw_1.Object.emba_Codcli[ll_Busca] 		= dw_embalaje.Object.emba_Codcli[ll_Fila]
			dw_1.Object.etiq_Codigo[ll_Busca] 		= dw_embalaje.Object.etiq_Codigo[ll_Fila]
			dw_1.Object.emba_Pesmin[ll_Busca] 	= dw_embalaje.Object.emba_Pesmin[ll_Fila]
			dw_1.Object.emba_Pesmax[ll_Busca] 	= dw_embalaje.Object.emba_Pesmax[ll_Fila]
			dw_1.Object.emba_Pesnet[ll_Busca] 		= dw_embalaje.Object.emba_Pesnet[ll_Fila]
			dw_1.Object.emba_Altura[ll_Busca] 		= dw_embalaje.Object.emba_Altura[ll_Fila]
			dw_1.Object.emba_Columna[ll_Busca] 	= dw_embalaje.Object.emba_Columna[ll_Fila]
//			dw_1.Object.emba_Nrogs1[ll_Busca] 		= dw_embalaje.Object.emba_Nrogs1[ll_Fila]
//			dw_1.Object.emba_Nroint[ll_Busca] 		= dw_embalaje.Object.emba_Nroint[ll_Fila]
//			dw_1.Object.emba_Usapes[ll_Busca] 	= dw_embalaje.Object.emba_Usapes[ll_Fila]
			dw_1.Object.emba_Tipbol[ll_Busca] 		= dw_embalaje.Object.emba_Tipbol[ll_Fila]
			dw_1.Object.emba_Tipvid[ll_Busca] 		= dw_embalaje.Object.emba_Tipvid[ll_Fila]
			dw_1.Object.emba_Codant[ll_Busca] 		= dw_embalaje.Object.emba_Codant[ll_Fila]
//			dw_1.Object.emba_Perten[ll_Busca] 		= dw_embalaje.Object.emba_Perten[ll_Fila]
			dw_1.Object.emba_Flgexp[ll_Busca] 		= dw_embalaje.Object.emba_Flgexp[ll_Fila]
			dw_1.Object.emba_Vigent[ll_Busca] 		= dw_embalaje.Object.emba_Vigent[ll_Fila]
			dw_1.Object.emba_Tipofr[ll_Busca] 		= dw_embalaje.Object.emba_Tipofr[ll_Fila]
			dw_1.Object.emba_Premium[ll_Busca] 	= dw_embalaje.Object.emba_Premium[ll_Fila]
			dw_1.Object.emba_Flggra[ll_Busca] 		= dw_embalaje.Object.emba_Flggra[ll_Fila]
			dw_1.Object.emba_Flgvar[ll_Busca] 		= dw_embalaje.Object.emba_Flgvar[ll_Fila]
			dw_1.Object.emba_Flgpsn[ll_Busca] 		= dw_embalaje.Object.emba_Flgpsn[ll_Fila]
			dw_1.Object.copa_Codigo[ll_Busca] 		= dw_embalaje.Object.copa_Codigo[ll_Fila]
			dw_1.Object.emba_Flgusn[ll_Busca] 		= dw_embalaje.Object.emba_Flgusn[ll_Fila]
			dw_1.Object.emba_Atmosf[ll_Busca] 		= dw_embalaje.Object.emba_Atmosf[ll_Fila]
			dw_1.Object.emba_Descri[ll_Busca] 		= dw_embalaje.Object.emba_Descri[ll_Fila]
			wf_Inserta('Se actualiza embalaje:'  + Upper(embalaje))
		End If
	Next
	If wf_Graba(False) Then wf_Inserta('Se grabo embalaje:'  + Upper(embalaje))
Else
	wf_Inserta('Embalaje ' + Upper(embalaje) + ', No existe...')
End If

Destroy lnv_RestClient

Return lb_Retorno
end function

public subroutine wf_inserta (string mensaje);If IsNull(Mensaje) Then Mensaje = ''

mle_Texto.Text += String(Today(), 'dd/mm/yyyy hh:mm:ss') + ' - ' + Mensaje + '~r~n'
mle_Texto.Scroll(mle_Texto.LineCount())

end subroutine

public function boolean wf_tipopallemba (long cliente, string embalaje);Boolean 		lb_Retorno = True
RESTClient	lnv_RestClient
String 		ls_URL, ls_Busca
Long			ll_Busca, ll_New, ll_Fila


wf_Inserta('Proceso de creacion de Altura de Pallet...')
wf_Inserta('Recupera Altura para embalaje: ' + Upper(embalaje))

lnv_RestClient	=	Create RESTClient
lnv_RestClient.SetRequestHeader ("Content-Type", "application/json;charset=UTF-8")

dw_2.DataObject = 'dw_mues_tipopallemba'
ls_URL = _URL + 'api/Tipopallemba/Retrieve/' + String(Cliente) +'/' + Embalaje
lnv_RestClient.Retrieve(dw_2, ls_URL)

If dw_2.RowCount() > 0 Then
	
	dw_1.DataObject = 'dw_mues_tipopallemba'
	dw_1.SetTransObject(SQLCA)
	dw_1.Retrieve(Cliente, Embalaje)
		
	For ll_Fila = 1 To dw_2.RowCount()
		ls_Busca = 'emba_codigo = "'  + Trim(Embalaje)+ '" And tpem_Codigo = "' + dw_2.Object.tpem_codigo[ll_Fila] + '"'
		ll_Busca = dw_1.Find(ls_Busca, 1, dw_1.RowCount(), Primary!)
	
		If ll_Busca = 0 Then
			dw_2.RowsCopy(ll_Fila, ll_Fila, Primary!, dw_1, 1, Primary!)
			wf_Inserta('Se inserta Altura embalaje:'  + embalaje)
		Else
			dw_1.Object.tpem_Cancaj[ll_Busca] 		= dw_2.Object.tpem_Cancaj[ll_Fila]
			dw_1.Object.tpem_Altura[ll_Busca] 		= dw_2.Object.tpem_Altura[ll_Fila]
			dw_1.Object.emba_Nombre[ll_Busca]	= dw_2.Object.emba_Nombre[ll_Fila]
			dw_1.Object.emba_Altura[ll_Busca] 		= dw_2.Object.emba_Altura[ll_Fila]
			dw_1.Object.emba_Columna[ll_Busca] 	= dw_2.Object.emba_Columna[ll_Fila]

			wf_Inserta('Se actualiza altura para embalaje:'  + Upper(embalaje))
		End If
	Next
	If wf_Graba(False) Then wf_Inserta('Se grabo altura para embalaje:'  + Upper(embalaje))
Else
	wf_Inserta('Altura para Embalaje ' + Upper(embalaje) + ', No existe...')
End If

Destroy lnv_RestClient

Return lb_Retorno
end function

public function boolean wf_gtin (long cliente, string embalaje);Boolean 		lb_Retorno = True
RESTClient	lnv_RestClient
String 		ls_URL, ls_Busca
Long			ll_Busca, ll_New, ll_Fila


wf_Inserta('Proceso de creacion de GTIN de Pallet...')
wf_Inserta('Recupera GTIN para embalaje: ' + Upper(embalaje))

lnv_RestClient	=	Create RESTClient
lnv_RestClient.SetRequestHeader ("Content-Type", "application/json;charset=UTF-8")

dw_2.DataObject = 'dw_mant_mues_gtin_dun14'
ls_URL = _URL + 'api/Gtin_Dun14/Retrieve/' + String(Cliente) +'/' + Embalaje
lnv_RestClient.Retrieve(dw_2, ls_URL)

If dw_2.RowCount() > 0 Then
	
	dw_1.DataObject = 'dw_mant_mues_gtin_dun14'
	dw_1.SetTransObject(SQLCA)
	dw_1.Retrieve(Cliente, Embalaje)
	For ll_Fila = 1 To dw_2.RowCount()		  
		ls_Busca = 'emba_codigo = "'  + Trim(Embalaje)+ '" And Gtin_Calibr = "' + dw_2.Object.Gtin_Calibr[ll_Fila] + '" and espe_codigo = ' + &
							String(dw_2.Object.Espe_Codigo[ll_Fila]) + ' And vari_codigo = ' + String(dw_2.Object.Vari_Codigo[ll_Fila])
		ll_Busca = dw_1.Find(ls_Busca, 1, dw_1.RowCount(), Primary!)
	
		If ll_Busca = 0 Then
			dw_2.RowsCopy(ll_Fila, ll_Fila, Primary!, dw_1, 1, Primary!)
			wf_Inserta('Se inserta GTIN embalaje:'  + embalaje)
		Else
			dw_1.Object.Gtin_Numero[ll_Busca]	= dw_2.Object.Gtin_Numero[ll_Fila]
			dw_1.Object.Gtin_Codplu[ll_Busca]	= dw_2.Object.Gtin_Codplu[ll_Fila]
			dw_1.Object.Gtin_Codupc[ll_Busca]	= dw_2.Object.Gtin_Codupc[ll_Fila]

			wf_Inserta('Se actualiza GTIN para embalaje:'  + Upper(embalaje))
		End If
	Next
	If wf_Graba(False) Then wf_Inserta('Se grabo altura para embalaje:'  + Upper(embalaje))
Else
	wf_Inserta('GTIN para Embalaje ' + Upper(embalaje) + ', No existe...')
End If

Destroy lnv_RestClient

Return lb_Retorno
end function

on w_carga_json.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.uo_selcliente=create uo_selcliente
this.st_2=create st_2
this.dw_1=create dw_1
this.dw_2=create dw_2
this.mle_texto=create mle_texto
this.sle_embalaje=create sle_embalaje
this.dw_embalaje=create dw_embalaje
this.cb_1=create cb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.dw_1
this.Control[iCurrent+6]=this.dw_2
this.Control[iCurrent+7]=this.mle_texto
this.Control[iCurrent+8]=this.sle_embalaje
this.Control[iCurrent+9]=this.dw_embalaje
this.Control[iCurrent+10]=this.cb_1
end on

on w_carga_json.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.uo_selcliente)
destroy(this.st_2)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.mle_texto)
destroy(this.sle_embalaje)
destroy(this.dw_embalaje)
destroy(this.cb_1)
end on

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_carga_json
end type

type st_computador from w_para_informes`st_computador within w_carga_json
end type

type st_usuario from w_para_informes`st_usuario within w_carga_json
end type

type st_temporada from w_para_informes`st_temporada within w_carga_json
end type

type p_logo from w_para_informes`p_logo within w_carga_json
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_carga_json
integer width = 1751
string text = "Carga de Informacion de Embalajes"
end type

type pb_acepta from w_para_informes`pb_acepta within w_carga_json
string tag = "Generacion de Archivos XML"
integer x = 2121
integer y = 416
integer taborder = 30
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
string powertiptext = "Generacion de Archivos XML"
end type

event pb_acepta::clicked;call super::clicked;mle_Texto.Setfocus()
mle_Texto.Text = ''

If IsNull(sle_Embalaje.Text) Or sle_Embalaje.Text = '' Then
	wf_Inserta('Debe Ingresar un embalaje para procesar...')
	Return
Else
	wf_Embalaje(sle_Embalaje.Text)
	wf_TipoPallEmba(uo_SelCliente.Codigo, sle_Embalaje.Text)
	wf_Gtin(uo_SelCliente.Codigo, sle_Embalaje.Text)
	wf_Inserta('Proceso terminado...')
End If

//dw_1.DataObject = 'dw_mues_correlfolio'

//ls_URL = 'https://localhost:7559/api/Correlfolio/Retrieve/81'

end event

type pb_salir from w_para_informes`pb_salir within w_carga_json
integer x = 2121
integer y = 692
integer taborder = 40
string powertiptext = "Salir de la Ventana"
end type

type st_4 from statictext within w_carga_json
integer x = 247
integer y = 432
integer width = 1751
integer height = 1088
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_carga_json
integer x = 379
integer y = 508
integer width = 311
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Clliente"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_carga_json
event destroy ( )
integer x = 777
integer y = 492
integer height = 100
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_2 from statictext within w_carga_json
integer x = 379
integer y = 624
integer width = 311
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Embalaje"
boolean focusrectangle = false
end type

type dw_1 from uo_dw within w_carga_json
boolean visible = false
integer x = 2107
integer y = 920
integer width = 215
integer height = 164
integer taborder = 11
boolean bringtotop = true
boolean enabled = false
boolean vscrollbar = false
end type

type dw_2 from uo_dw within w_carga_json
boolean visible = false
integer x = 2103
integer y = 1108
integer width = 215
integer height = 164
integer taborder = 21
boolean bringtotop = true
boolean enabled = false
boolean vscrollbar = false
end type

type mle_texto from multilineedit within w_carga_json
integer x = 311
integer y = 760
integer width = 1650
integer height = 700
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type sle_embalaje from singlelineedit within w_carga_json
integer x = 777
integer y = 600
integer width = 402
integer height = 112
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

type dw_embalaje from uo_dw within w_carga_json
boolean visible = false
integer x = 2103
integer y = 1296
integer width = 215
integer height = 164
integer taborder = 31
boolean bringtotop = true
boolean enabled = false
boolean vscrollbar = false
borderstyle borderstyle = stylebox!
end type

type cb_1 from commandbutton within w_carga_json
boolean visible = false
integer x = 782
integer y = 168
integer width = 402
integer height = 112
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "none"
end type

event clicked;RESTClient inv_RestClient
inv_RestClient	= CREATE RESTClient

// Set the Request Headers to tell the Web API you will send JSON data
inv_RESTClient.SetRequestHeader ("Content-Type", "application/json;charset=UTF-8")

String ls_url_token = "https://localhost:7559/api/User"
String ls_user = '{"id":1, "UserName":"user12", "Password":"password1", "Role":"admin", "Token":""}'
String ls_token

// Get a JWT token.
If inv_RestClient.GetJWTToken(ls_url_token, ls_user, ls_token) = 1 Then

    // Set the JWT token string to the HTTP request header.
    inv_RestClient.SetJWTToken(ls_token)
Else
    // Get JWT token failed.
    Return
End If

String ls_url = "https://localhost:7559/api/Envases/RetrieveOne/2/86"
String ls_response
String ls_msg

// Request WeatherForecast
inv_RestClient.SendGetRequest(ls_url, ls_response)

// Show response info.
ls_msg = "Status Code: " + String(inv_RestClient.GetResponseStatusCode()) + '~r~n' + &
            "Status Text: " + String(inv_RestClient.GetResponseStatusText()) + '~r~n' + &
            "Response Body: " + ls_response

Messagebox("Response Info", ls_msg)


end event

