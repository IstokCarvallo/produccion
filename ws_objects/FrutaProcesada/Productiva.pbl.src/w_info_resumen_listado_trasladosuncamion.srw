$PBExportHeader$w_info_resumen_listado_trasladosuncamion.srw
forward
global type w_info_resumen_listado_trasladosuncamion from w_para_informes
end type
type st_4 from statictext within w_info_resumen_listado_trasladosuncamion
end type
type st_1 from statictext within w_info_resumen_listado_trasladosuncamion
end type
type st_6 from statictext within w_info_resumen_listado_trasladosuncamion
end type
type st_2 from statictext within w_info_resumen_listado_trasladosuncamion
end type
type em_operacion from editmask within w_info_resumen_listado_trasladosuncamion
end type
type cb_oper from uo_buscar within w_info_resumen_listado_trasladosuncamion
end type
type sle_nave from singlelineedit within w_info_resumen_listado_trasladosuncamion
end type
type em_fzarpe from editmask within w_info_resumen_listado_trasladosuncamion
end type
type st_3 from statictext within w_info_resumen_listado_trasladosuncamion
end type
type em_nroguia from editmask within w_info_resumen_listado_trasladosuncamion
end type
type st_5 from statictext within w_info_resumen_listado_trasladosuncamion
end type
type cbx_pallet from checkbox within w_info_resumen_listado_trasladosuncamion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_resumen_listado_trasladosuncamion
end type
type uo_selplanta from uo_seleccion_plantas within w_info_resumen_listado_trasladosuncamion
end type
end forward

global type w_info_resumen_listado_trasladosuncamion from w_para_informes
integer x = 14
integer y = 32
integer width = 2990
integer height = 1492
string title = "Resumen de Traslados un Camión"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_6 st_6
st_2 st_2
em_operacion em_operacion
cb_oper cb_oper
sle_nave sle_nave
em_fzarpe em_fzarpe
st_3 st_3
em_nroguia em_nroguia
st_5 st_5
cbx_pallet cbx_pallet
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_info_resumen_listado_trasladosuncamion w_info_resumen_listado_trasladosuncamion

type variables




end variables

forward prototypes
public function boolean existeoperacion (string operacion)
end prototypes

public function boolean existeoperacion (string operacion);String		ls_nave
Date		ld_fzarpe

IF operacion <> "" THEN
	
	SELECT	embq_nomnav, embq_fzarpe
		INTO	:ls_nave, :ld_fzarpe
		FROM	dbo.EMBARQUEPROD
		WHERE	clie_codigo	=	:uo_SelCliente.Codigo
		AND	embq_codigo =	:operacion ;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla EMBARQUEPROD")
		em_operacion.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Operación Indicada.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_acepta.Enabled	= False
		em_operacion.SetFocus()
		RETURN False
	ELSE
		sle_nave.text		= ls_nave
		em_fzarpe.text		= String(ld_fzarpe)
		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

on w_info_resumen_listado_trasladosuncamion.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_6=create st_6
this.st_2=create st_2
this.em_operacion=create em_operacion
this.cb_oper=create cb_oper
this.sle_nave=create sle_nave
this.em_fzarpe=create em_fzarpe
this.st_3=create st_3
this.em_nroguia=create em_nroguia
this.st_5=create st_5
this.cbx_pallet=create cbx_pallet
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_operacion
this.Control[iCurrent+6]=this.cb_oper
this.Control[iCurrent+7]=this.sle_nave
this.Control[iCurrent+8]=this.em_fzarpe
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.em_nroguia
this.Control[iCurrent+11]=this.st_5
this.Control[iCurrent+12]=this.cbx_pallet
this.Control[iCurrent+13]=this.uo_selcliente
this.Control[iCurrent+14]=this.uo_selplanta
end on

on w_info_resumen_listado_trasladosuncamion.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_6)
destroy(this.st_2)
destroy(this.em_operacion)
destroy(this.cb_oper)
destroy(this.sle_nave)
destroy(this.em_fzarpe)
destroy(this.st_3)
destroy(this.em_nroguia)
destroy(this.st_5)
destroy(this.cbx_pallet)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event open;Boolean	lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Filtra(1)
	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_resumen_listado_trasladosuncamion
integer x = 2482
integer y = 436
end type

type st_computador from w_para_informes`st_computador within w_info_resumen_listado_trasladosuncamion
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_listado_trasladosuncamion
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_listado_trasladosuncamion
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_listado_trasladosuncamion
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_listado_trasladosuncamion
integer width = 2135
string text = "Resumen de Traslados un Camión"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_listado_trasladosuncamion
integer x = 2491
integer y = 704
integer taborder = 60
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	li_fila, li_pallet
Long		ll_guia
String		ls_embarque

istr_info.titulo	= 'DETALLE DE DESPACHO POR CAMION'

ls_embarque	=	em_operacion.Text
ll_guia			=	Long(em_nroguia.Text)

OpenWithParm(vinf,istr_info)

IF cbx_pallet.Checked THEN
	vinf.dw_1.DataObject = "dw_info_detalle_listado_trasladosuncamion"
	li_pallet	=	1
ELSE
	vinf.dw_1.DataObject = "dw_info_resumen_listado_trasladosuncamion"
	li_pallet	=	0
END IF

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ls_embarque, ll_guia, li_pallet)
								  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("t_guia.text = '" + em_nroguia.text + "'")
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_listado_trasladosuncamion
integer x = 2487
integer y = 984
integer taborder = 70
end type

type st_4 from statictext within w_info_resumen_listado_trasladosuncamion
integer x = 251
integer y = 440
integer width = 2135
integer height = 600
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_resumen_listado_trasladosuncamion
integer x = 343
integer y = 592
integer width = 448
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

type st_6 from statictext within w_info_resumen_listado_trasladosuncamion
integer x = 343
integer y = 500
integer width = 448
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

type st_2 from statictext within w_info_resumen_listado_trasladosuncamion
integer x = 334
integer y = 692
integer width = 443
integer height = 84
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
string text = "Nro Embarque"
boolean focusrectangle = false
end type

type em_operacion from editmask within w_info_resumen_listado_trasladosuncamion
integer x = 951
integer y = 672
integer width = 261
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

event getfocus;IF This.Text <> '' THEN
	IF ExisteOperacion(This.Text) THEN
	END IF	
END IF	
end event

event modified;IF ExisteOperacion(This.Text) = False THEN
	This.SetFocus()
END IF



 
	
end event

type cb_oper from uo_buscar within w_info_resumen_listado_trasladosuncamion
integer x = 1221
integer y = 676
integer width = 96
integer height = 88
integer taborder = 40
boolean bringtotop = true
end type

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	String(uo_SelCliente.Codigo)

OpenWithParm(w_busc_embarques, lstr_busq)

lstr_busq	= Message.PowerObjectParm

If lstr_busq.argum[1] = "" Then
	em_operacion.SetFocus()
ElseIf UpperBound(lstr_busq.argum) > 2 Then	
	em_operacion.Text		= lstr_busq.argum[1]
	sle_nave.text				= lstr_busq.argum[2]
	em_fzarpe.text				= lstr_busq.argum[3]
	pb_acepta.Enabled		= True
End If



end event

type sle_nave from singlelineedit within w_info_resumen_listado_trasladosuncamion
integer x = 951
integer y = 780
integer width = 1285
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type em_fzarpe from editmask within w_info_resumen_listado_trasladosuncamion
integer x = 1353
integer y = 676
integer width = 430
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

type st_3 from statictext within w_info_resumen_listado_trasladosuncamion
integer x = 306
integer y = 912
integer width = 325
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
string text = "Nro. Guia"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_nroguia from editmask within w_info_resumen_listado_trasladosuncamion
integer x = 951
integer y = 892
integer width = 402
integer height = 96
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;//IF ExisteGuia(Long(This.Text)) = False THEN
//	This.SetFocus()
//END IF
end event

type st_5 from statictext within w_info_resumen_listado_trasladosuncamion
integer x = 251
integer y = 1040
integer width = 2135
integer height = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_pallet from checkbox within w_info_resumen_listado_trasladosuncamion
integer x = 690
integer y = 1088
integer width = 1253
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Detallado por PALLET - Temperaturas    "
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_resumen_listado_trasladosuncamion
event destroy ( )
integer x = 946
integer y = 488
integer height = 92
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_resumen_listado_trasladosuncamion
event destroy ( )
integer x = 946
integer y = 584
integer height = 92
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

