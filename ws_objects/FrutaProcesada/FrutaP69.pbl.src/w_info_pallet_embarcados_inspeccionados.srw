$PBExportHeader$w_info_pallet_embarcados_inspeccionados.srw
forward
global type w_info_pallet_embarcados_inspeccionados from w_para_informes
end type
type st_1 from statictext within w_info_pallet_embarcados_inspeccionados
end type
type st_3 from statictext within w_info_pallet_embarcados_inspeccionados
end type
type dw_10 from datawindow within w_info_pallet_embarcados_inspeccionados
end type
type st_4 from statictext within w_info_pallet_embarcados_inspeccionados
end type
type dw_11 from datawindow within w_info_pallet_embarcados_inspeccionados
end type
type st_2 from statictext within w_info_pallet_embarcados_inspeccionados
end type
type em_operacion from editmask within w_info_pallet_embarcados_inspeccionados
end type
type cb_oper from uo_buscar within w_info_pallet_embarcados_inspeccionados
end type
type sle_nave from singlelineedit within w_info_pallet_embarcados_inspeccionados
end type
type em_fzarpe from editmask within w_info_pallet_embarcados_inspeccionados
end type
end forward

global type w_info_pallet_embarcados_inspeccionados from w_para_informes
integer x = 14
integer y = 32
integer width = 2747
integer height = 1684
string title = "Pallets Despachos/Inspeccionados"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_3 st_3
dw_10 dw_10
st_4 st_4
dw_11 dw_11
st_2 st_2
em_operacion em_operacion
cb_oper cb_oper
sle_nave sle_nave
em_fzarpe em_fzarpe
end type
global w_info_pallet_embarcados_inspeccionados w_info_pallet_embarcados_inspeccionados

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_especie
String is_fecha_emb
Integer ii_Especie, ii_Cliente, ii_Planta, ii_año, ii_mes, ii_dia, ii_var
Long	il_NroGuia
end variables

forward prototypes
public function boolean existeoperacion (string operacion)
end prototypes

public function boolean existeoperacion (string operacion);Integer	li_codexp
String	ls_nave, ls_embarque
Date		ld_fzarpe

li_codexp		=	dw_10.GetItemNumber(1,"clie_codigo")
ls_embarque		=	operacion

IF ls_embarque <> "" THEN
	
	SELECT	embq_nomnav, embq_fzarpe
		INTO	:ls_nave, :ld_fzarpe
		FROM	dba.EMBARQUEPROD
		WHERE	clie_codigo	=	:li_codexp
		AND	embq_codigo =	:ls_embarque ;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla EMBARQUEPROD")
		em_operacion.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Operación Indicada.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_operacion.SetFocus()
		RETURN False
	ELSE
		istr_mant.argumento[3]	=	operacion
		sle_nave.text				= ls_nave
		em_fzarpe.text				= String(ld_fzarpe)
		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

on w_info_pallet_embarcados_inspeccionados.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_3=create st_3
this.dw_10=create dw_10
this.st_4=create st_4
this.dw_11=create dw_11
this.st_2=create st_2
this.em_operacion=create em_operacion
this.cb_oper=create cb_oper
this.sle_nave=create sle_nave
this.em_fzarpe=create em_fzarpe
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.dw_10
this.Control[iCurrent+4]=this.st_4
this.Control[iCurrent+5]=this.dw_11
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.em_operacion
this.Control[iCurrent+8]=this.cb_oper
this.Control[iCurrent+9]=this.sle_nave
this.Control[iCurrent+10]=this.em_fzarpe
end on

on w_info_pallet_embarcados_inspeccionados.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_3)
destroy(this.dw_10)
destroy(this.st_4)
destroy(this.dw_11)
destroy(this.st_2)
destroy(this.em_operacion)
destroy(this.cb_oper)
destroy(this.sle_nave)
destroy(this.em_fzarpe)
end on

event open;call super::open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

pb_acepta.Enabled	=	False

dw_10.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_10.InsertRow(0)
dw_10.SetItem(1,"clie_codigo",gi_codexport)

dw_11.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_11.InsertRow(0)
dw_11.SetItem(1,"plde_codigo",gi_codplanta)

istr_mant.argumento[1]	=	String(gi_codexport)
istr_mant.argumento[2]	=	String(gi_codplanta)

end event

type st_computador from w_para_informes`st_computador within w_info_pallet_embarcados_inspeccionados
end type

type st_usuario from w_para_informes`st_usuario within w_info_pallet_embarcados_inspeccionados
end type

type st_temporada from w_para_informes`st_temporada within w_info_pallet_embarcados_inspeccionados
end type

type p_logo from w_para_informes`p_logo within w_info_pallet_embarcados_inspeccionados
end type

type st_titulo from w_para_informes`st_titulo within w_info_pallet_embarcados_inspeccionados
integer width = 1934
string text = "Relación Pallets Despachados/Inspeccionados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_pallet_embarcados_inspeccionados
string tag = "Imprimir Reporte"
integer x = 2363
integer y = 800
integer taborder = 50
integer weight = 400
fontcharset fontcharset = ansi!
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta
String	ls_cliente, ls_planta, ls_embarque

istr_info.titulo	= 'RELACION PALLET DESPACHADOS/INSPECCIONADOS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_pallet_embarc_inspeccionados"
vinf.dw_1.SetTransObject(sqlca)

ls_embarque	=	em_operacion.text + " " + sle_nave.text

ls_cliente	=	idwc_cliente.GetItemString(idwc_cliente.GetRow(),"clie_nombre")
ls_planta	=	idwc_planta.GetItemString(idwc_planta.GetRow(),"plde_nombre")

fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),istr_mant.argumento[3])

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("cliente.text = '" + ls_cliente + "'")
		vinf.dw_1.Modify("planta.text = '" + ls_planta + "'")
		vinf.dw_1.Modify("embarque.text = '" + ls_embarque + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_pallet_embarcados_inspeccionados
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2359
integer y = 1084
integer taborder = 60
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_pallet_embarcados_inspeccionados
integer x = 247
integer y = 440
integer width = 1925
integer height = 856
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_pallet_embarcados_inspeccionados
integer x = 343
integer y = 568
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_10 from datawindow within w_info_pallet_embarcados_inspeccionados
integer x = 791
integer y = 544
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	data
idwc_planta.Retrieve(1)
istr_mant.argumento[2]	=	String(dw_11.Object.plde_codigo[1])
dw_11.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))

end event

type st_4 from statictext within w_info_pallet_embarcados_inspeccionados
integer x = 343
integer y = 720
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_11 from datawindow within w_info_pallet_embarcados_inspeccionados
integer x = 791
integer y = 696
integer width = 969
integer height = 100
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]	=	data
end event

type st_2 from statictext within w_info_pallet_embarcados_inspeccionados
integer x = 343
integer y = 868
integer width = 443
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Nro Embarque"
boolean focusrectangle = false
end type

type em_operacion from editmask within w_info_pallet_embarcados_inspeccionados
integer x = 791
integer y = 852
integer width = 261
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

event getfocus;//IF ExisteOperacion(This.Text) 
end event

event modified;IF ExisteOperacion(This.Text) = False THEN
	This.SetFocus()
ELSE
	pb_acepta.Enabled	=	True

END IF



 
	
end event

type cb_oper from uo_buscar within w_info_pallet_embarcados_inspeccionados
integer x = 1074
integer y = 852
integer width = 96
integer height = 88
integer taborder = 40
boolean bringtotop = true
end type

event clicked;call super::clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1]

OpenWithParm(w_busc_embarques, lstr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[1] = "" THEN
	em_operacion.SetFocus()
ELSE
	em_operacion.Text			= 	istr_busq.argum[1]
	sle_nave.text				= 	istr_busq.argum[2]
	em_fzarpe.text				= 	istr_busq.argum[3]
	pb_acepta.Enabled			=	True
	istr_mant.argumento[3]	=	istr_busq.argum[1]
END IF



end event

type sle_nave from singlelineedit within w_info_pallet_embarcados_inspeccionados
integer x = 343
integer y = 1008
integer width = 1285
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type em_fzarpe from editmask within w_info_pallet_embarcados_inspeccionados
integer x = 1664
integer y = 1008
integer width = 357
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

