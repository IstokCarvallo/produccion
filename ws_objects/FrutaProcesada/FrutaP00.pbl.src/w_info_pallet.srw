$PBExportHeader$w_info_pallet.srw
$PBExportComments$Informe de Revisión Repalletizajes.
forward
global type w_info_pallet from w_para_informes
end type
type st_4 from statictext within w_info_pallet
end type
type st_5 from statictext within w_info_pallet
end type
type st_2 from statictext within w_info_pallet
end type
type em_numero from editmask within w_info_pallet
end type
type dw_2 from datawindow within w_info_pallet
end type
type st_6 from statictext within w_info_pallet
end type
type cb_buscarepa from commandbutton within w_info_pallet
end type
type st_1 from statictext within w_info_pallet
end type
type dw_planta from datawindow within w_info_pallet
end type
end forward

global type w_info_pallet from w_para_informes
integer x = 14
integer y = 32
integer width = 2747
integer height = 1444
string title = "Procesos Históricos Pallet"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_5 st_5
st_2 st_2
em_numero em_numero
dw_2 dw_2
st_6 st_6
cb_buscarepa cb_buscarepa
st_1 st_1
dw_planta dw_planta
end type
global w_info_pallet w_info_pallet

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente, idwc_planta

Integer	ii_planta
end variables

forward prototypes
public function boolean noexisteplanta (integer planta)
end prototypes

public function boolean noexisteplanta (integer planta);Integer		li_planta
String		ls_nombre

li_planta	=	Integer(planta)

SELECT	plde_nombre
	INTO	:ls_nombre
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:li_planta;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla PlantaDesp")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	ii_planta	=	(li_planta)	
	RETURN False
END IF
end function

on w_info_pallet.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_5=create st_5
this.st_2=create st_2
this.em_numero=create em_numero
this.dw_2=create dw_2
this.st_6=create st_6
this.cb_buscarepa=create cb_buscarepa
this.st_1=create st_1
this.dw_planta=create dw_planta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_5
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_numero
this.Control[iCurrent+5]=this.dw_2
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.cb_buscarepa
this.Control[iCurrent+8]=this.st_1
this.Control[iCurrent+9]=this.dw_planta
end on

on w_info_pallet.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.cb_buscarepa)
destroy(this.st_1)
destroy(this.dw_planta)
end on

event open;call super::open;String	ls_Columna[]

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1, "plde_codigo", gi_codplanta)

ii_planta = gi_codplanta

IF ExisteCliente(gi_CodExport, ls_Columna[]) THEN
	istr_mant.argumento[3]	=	ls_Columna[1]
END IF

istr_mant.argumento[1]	=	String(gi_codexport)
istr_mant.argumento[2]	=	'0'

end event

type pb_excel from w_para_informes`pb_excel within w_info_pallet
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type st_computador from w_para_informes`st_computador within w_info_pallet
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_usuario from w_para_informes`st_usuario within w_info_pallet
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_temporada from w_para_informes`st_temporada within w_info_pallet
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type p_logo from w_para_informes`p_logo within w_info_pallet
end type

type st_titulo from w_para_informes`st_titulo within w_info_pallet
integer width = 1902
string text = "Detalle de un  Pallet"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_pallet
integer x = 2331
integer y = 480
integer taborder = 30
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta, li_varirotula
Long		li_planta,li_cliente
String	ls_region

IF Long(istr_mant.argumento[2]) = 0 THEN

		MessageBox("Atención", "Falta Número del Pallet.", &
						Exclamation!, Ok!)
		em_numero.SetFocus()

ELSE
		istr_info.titulo	= 'INFORME DETALLE DE UN PALLET.'

		OpenWithParm(vinf, istr_info)
		
		vinf.dw_1.DataObject = "dw_info_palletfruta_palletenca"

		vinf.dw_1.SetTransObject(sqlca)
		
		fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]),ii_planta,Long(istr_mant.Argumento[2]))
											
		IF fila = -1 THEN
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
							"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		
		ELSEIF fila = 0 THEN
			MessageBox( "No Existe información", "No existe información para este informe.", &
							 StopSign!, Ok!)
		
		ELSE
			F_Membrete(vinf.dw_1)
			
			vinf.dw_1.Modify("cliente.text = '" + istr_mant.Argumento[3] + "'")
			vinf.dw_1.Modify("pallet.text = '" + istr_mant.Argumento[2] + "'")			
			IF gs_Ambiente <> 'Windows' THEN
				F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
			END IF
		END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_pallet
integer x = 2327
integer y = 752
integer taborder = 40
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type st_4 from statictext within w_info_pallet
integer x = 251
integer y = 440
integer width = 1902
integer height = 404
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_pallet
integer x = 251
integer y = 844
integer width = 1902
integer height = 236
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_pallet
integer x = 302
integer y = 948
integer width = 517
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
string text = "Número Pallet"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_pallet
integer x = 887
integer y = 920
integer width = 462
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[2]	=	This.Text

end event

type dw_2 from datawindow within w_info_pallet
integer x = 882
integer y = 512
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	istr_mant.argumento[3]	=	ls_Columna[1]
	
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_pallet
integer x = 297
integer y = 520
integer width = 233
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
string text = "Cliente"
boolean focusrectangle = false
end type

type cb_buscarepa from commandbutton within w_info_pallet
integer x = 1390
integer y = 924
integer width = 91
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "..."
end type

event clicked;istr_busq.argum[1]	=	istr_mant.argumento[1]
istr_busq.argum[5]   = ''

OpenWithParm(w_busc_palletencab, istr_busq)

istr_busq = Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	istr_mant.argumento[2] 	=	istr_busq.argum[2]
	em_numero.Text 			=	istr_busq.argum[2]
END IF
end event

type st_1 from statictext within w_info_pallet
integer x = 297
integer y = 712
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_pallet
integer x = 873
integer y = 668
integer width = 969
integer height = 100
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;IF NoExisteplanta(Integer(data)) THEN
	dw_planta.SetItem(1, "plde_codigo", gi_codplanta)
	dw_planta.SetFocus()
	RETURN 1
END IF

//ii_planta =	Integer(data)

end event

event itemerror;RETURN 1
end event

