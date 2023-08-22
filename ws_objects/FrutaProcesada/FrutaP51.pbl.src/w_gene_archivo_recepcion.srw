$PBExportHeader$w_gene_archivo_recepcion.srw
$PBExportComments$Genera archivo Plano SAG por Recepciones de Pallets Inter Planta.
forward
global type w_gene_archivo_recepcion from window
end type
type uo_selplanta from uo_seleccion_plantas within w_gene_archivo_recepcion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_recepcion
end type
type dw_3 from datawindow within w_gene_archivo_recepcion
end type
type st_numero from statictext within w_gene_archivo_recepcion
end type
type em_nrosag from editmask within w_gene_archivo_recepcion
end type
type st_6 from statictext within w_gene_archivo_recepcion
end type
type st_4 from statictext within w_gene_archivo_recepcion
end type
type st_3 from statictext within w_gene_archivo_recepcion
end type
type sle_mensa from singlelineedit within w_gene_archivo_recepcion
end type
type em_fecha from editmask within w_gene_archivo_recepcion
end type
type cb_numero from uo_buscar within w_gene_archivo_recepcion
end type
type em_numero from editmask within w_gene_archivo_recepcion
end type
type dw_1 from datawindow within w_gene_archivo_recepcion
end type
type st_5 from statictext within w_gene_archivo_recepcion
end type
type st_2 from statictext within w_gene_archivo_recepcion
end type
type st_1 from statictext within w_gene_archivo_recepcion
end type
type pb_salir from picturebutton within w_gene_archivo_recepcion
end type
type pb_grabar from picturebutton within w_gene_archivo_recepcion
end type
type dw_2 from datawindow within w_gene_archivo_recepcion
end type
end forward

global type w_gene_archivo_recepcion from window
integer width = 2638
integer height = 1296
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
dw_3 dw_3
st_numero st_numero
em_nrosag em_nrosag
st_6 st_6
st_4 st_4
st_3 st_3
sle_mensa sle_mensa
em_fecha em_fecha
cb_numero cb_numero
em_numero em_numero
dw_1 dw_1
st_5 st_5
st_2 st_2
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
dw_2 dw_2
end type
global w_gene_archivo_recepcion w_gene_archivo_recepcion

type variables
str_mant			istr_mant
str_busqueda	istr_busq
Boolean			ib_Anulacion
Integer			ii_CantTarjas, ii_CantInspec
Date				id_FechaRepa
Date				id_FechaAcceso
Time				it_HoraAcceso
end variables

forward prototypes
public function string fechainspeccion (long al_nropallet)
public function boolean noexistefolio (long al_numero)
end prototypes

event ue_guardar();Long			ll_Fila, ll_Filas, ll_FilaDet
String			ls_Archivo, ls_Registro, ls_NumeroSAG
dw_2.reset()
dw_1.reset()

dw_2.SetTransObject(Sqlca)

ls_NumeroSAG	=	String(Long(em_nrosag.Text), '00000')
ll_Filas		= dw_2.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Long(istr_mant.argumento[3]))
	
If ll_Filas = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Pallets")
ElseIf ll_Filas = 0 Then
	MessageBox("Atención", "No hay información con Recepcion indicado.~r~rIngrese otra Numero.", Exclamation!, Ok!)
	pb_grabar.Enabled	= False
	em_numero.SetFocus()
Else
	dw_2.SetSort('paen_numero')
	dw_2.Sort()

	If uo_SelPlanta.Region <> '4' Then
		ls_Archivo	=	String(uo_SelPlanta.PlantaSAG, '000') + ls_NumeroSAG + ".INT"
	Else
		ls_Archivo	=	String(uo_SelPlanta.PlantaSAG, '0000') + ls_NumeroSAG + ".INT"
	End If	
	
	ls_Registro	=	ls_NumeroSAG
	ls_Registro	+=	String(uo_SelPlanta.PlantaSAG, '0000') 
	ls_Registro	+=	String(ll_Filas, '0000')
	ll_FilaDet	=	dw_1.InsertRow(0)
	
	dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
	
	For ll_Fila = 1 To ll_Filas
		ls_Registro  = uo_SelCliente.ClienteRotulado
		ls_Registro	+=	String(dw_2.Object.paen_numero[ll_Fila], '0000000')
		ls_Registro	+=	String(dw_2.Object.paen_ccajas[ll_Fila], '0000')
		ls_Registro	+=String(dw_2.Object.espe_codsag[ll_Fila], '00000000')
		ls_Registro	+=	String(dw_2.Object.dest_codigo[ll_Fila], '000')
		ls_Registro	+=	String(dw_2.Object.inpd_fechai[ll_Fila], 'yyyymmdd')
		
		ll_FilaDet	=	dw_1.InsertRow(0)
		dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
	Next
End If

//	Fin de Archivo

ls_Registro	=	"&&" 
ll_FilaDet	=	dw_1.InsertRow(0)

dw_1.Object.registro[ll_FilaDet]	=	ls_Registro

If dw_1.SaveAs(gs_disco+":\GeneradosSAG\" + ls_Archivo, Text!, False) = -1 Then
	MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
Else
	sle_mensa.text	= "Archivo " + ls_Archivo + " Generado. Avise a Computación."
End If
end event

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function string fechainspeccion (long al_nropallet);String	ls_Codigo
Integer	li_Cliente, li_destino, li_Planta
Date		ld_fechai

li_Cliente	=	Integer(istr_mant.Argumento[1])
li_Planta	=	Integer(istr_mant.Argumento[2])

SELECT	inpd_fechai
	INTO	:ld_fechai
	FROM	dbo.inspecpaldet
	WHERE	clie_codigo	=	:li_Cliente
	AND   plde_codigo =  :li_Planta
	AND	paen_numero	=	:al_NroPallet ;

ls_Codigo	=	String(ld_fechai, 'yyyymmdd')

RETURN ls_Codigo
end function

public function boolean noexistefolio (long al_numero);Integer	li_TipoRece
Long		ll_Numero

ll_Numero	=	Long(em_numero.Text)

SELECT	rfpe_fecrec, rfpe_tipoen
	INTO	:id_FechaRepa, :li_TipoRece
	FROM	dbo.RECFRUPROCEE
	WHERE	plde_codigo	=	:uo_SelPlanta.Codigo
	AND	rfpe_numero	=	:al_Numero ;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Recpeción Fruta Procesada")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Recepción no ha sido Ingresado.~r~r" + &
					"Ingrese o seleccione otro Número.", Exclamation!, Ok!)		
	RETURN True
ELSEIF li_TipoRece = 2 OR li_TipoRece = 6 THEN
	em_fecha.Text	=	String(id_FechaRepa)
	em_nrosag.Enabled =	True
	RETURN False
ELSE
	MessageBox("Atención", "Número de Recepción no Corresponde a Interplanta.~r~r" + &
					"Ingrese o seleccione otro Número.", Exclamation!, Ok!)					
	RETURN True
END IF
end function

on w_gene_archivo_recepcion.create
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
this.dw_3=create dw_3
this.st_numero=create st_numero
this.em_nrosag=create em_nrosag
this.st_6=create st_6
this.st_4=create st_4
this.st_3=create st_3
this.sle_mensa=create sle_mensa
this.em_fecha=create em_fecha
this.cb_numero=create cb_numero
this.em_numero=create em_numero
this.dw_1=create dw_1
this.st_5=create st_5
this.st_2=create st_2
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.dw_2=create dw_2
this.Control[]={this.uo_selplanta,&
this.uo_selcliente,&
this.dw_3,&
this.st_numero,&
this.em_nrosag,&
this.st_6,&
this.st_4,&
this.st_3,&
this.sle_mensa,&
this.em_fecha,&
this.cb_numero,&
this.em_numero,&
this.dw_1,&
this.st_5,&
this.st_2,&
this.st_1,&
this.pb_salir,&
this.pb_grabar,&
this.dw_2}
end on

on w_gene_archivo_recepcion.destroy
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
destroy(this.dw_3)
destroy(this.st_numero)
destroy(this.em_nrosag)
destroy(this.st_6)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.sle_mensa)
destroy(this.em_fecha)
destroy(this.cb_numero)
destroy(this.em_numero)
destroy(this.dw_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.dw_2)
end on

event open;Boolean	lb_Cerrar = False

x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_codexport)
	uo_SelPlanta.Inicia(gi_codplanta)
	
	em_nrosag.Enabled	=	False
	
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
End If
end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type uo_selplanta from uo_seleccion_plantas within w_gene_archivo_recepcion
event destroy ( )
integer x = 750
integer y = 448
integer height = 92
integer taborder = 20
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_recepcion
event destroy ( )
integer x = 750
integer y = 300
integer height = 92
integer taborder = 10
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type dw_3 from datawindow within w_gene_archivo_recepcion
boolean visible = false
integer x = 2075
integer y = 64
integer width = 178
integer height = 140
integer taborder = 30
string title = "none"
string dataobject = "dw_mues_recfruproced"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_numero from statictext within w_gene_archivo_recepcion
integer x = 178
integer y = 748
integer width = 443
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Número S.A.G."
boolean focusrectangle = false
end type

type em_nrosag from editmask within w_gene_archivo_recepcion
integer x = 750
integer y = 744
integer width = 416
integer height = 92
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;
pb_grabar.Enabled = True

end event

type st_6 from statictext within w_gene_archivo_recepcion
integer x = 82
integer y = 920
integer width = 1970
integer height = 224
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

type st_4 from statictext within w_gene_archivo_recepcion
integer x = 178
integer y = 460
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_gene_archivo_recepcion
integer x = 178
integer y = 312
integer width = 311
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_gene_archivo_recepcion
integer x = 123
integer y = 972
integer width = 1888
integer height = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
boolean italic = true
long textcolor = 65535
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type em_fecha from editmask within w_gene_archivo_recepcion
integer x = 1289
integer y = 596
integer width = 416
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "~t/"
end type

type cb_numero from uo_buscar within w_gene_archivo_recepcion
event clicked pbm_bnclicked
integer x = 1179
integer y = 596
integer width = 96
integer height = 88
integer taborder = 50
boolean bringtotop = true
end type

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	String(uo_SelCliente.Codigo)
lstr_busq.argum[2]	=	String(uo_SelPlanta.Codigo)

OpenWithParm(w_busc_recfruprocee, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

If UpperBound(lstr_busq.Argum) > 4 Then
	If lstr_busq.argum[5] <> "" Then
		em_numero.Text	= lstr_busq.argum[5]
		NoExisteFolio(Long(lstr_busq.argum[5]))
		istr_mant.argumento[3]	=	lstr_busq.argum[5]
	Else
		em_numero.SetFocus()
	End If
End If
end event

type em_numero from editmask within w_gene_archivo_recepcion
event getfocus pbm_ensetfocus
event modified pbm_enmodified
integer x = 750
integer y = 596
integer width = 416
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "########"
string displaydata = "$"
end type

event modified;If IsNull(This.Text) Or This.Text = '' Then Return


IF NoExisteFolio(Long(This.Text)) THEN
	This.Text	=	""
	This.SetFocus()
ELSE
	istr_mant.argumento[3]	=	String(Long(This.Text), '00000000')
END IF
end event

type dw_1 from datawindow within w_gene_archivo_recepcion
boolean visible = false
integer x = 2263
integer y = 64
integer width = 242
integer height = 144
string dataobject = "dw_gene_archivo_saam_plano"
boolean livescroll = true
end type

type st_5 from statictext within w_gene_archivo_recepcion
integer x = 78
integer y = 68
integer width = 1970
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Genera Archivo Plano S.A.G. por Recepción"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_gene_archivo_recepcion
integer x = 178
integer y = 600
integer width = 535
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Nro. Recepción"
boolean focusrectangle = false
end type

type st_1 from statictext within w_gene_archivo_recepcion
integer x = 82
integer y = 224
integer width = 1970
integer height = 696
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

type pb_salir from picturebutton within w_gene_archivo_recepcion
integer x = 2135
integer y = 908
integer width = 302
integer height = 244
integer taborder = 80
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_archivo_recepcion
integer x = 2135
integer y = 612
integer width = 302
integer height = 244
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type dw_2 from datawindow within w_gene_archivo_recepcion
boolean visible = false
integer x = 2071
integer y = 212
integer width = 421
integer height = 236
string dataobject = "dw_generacion_arch_recepcion"
end type

event clicked;This.Print()
end event

