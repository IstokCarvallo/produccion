$PBExportHeader$w_info_despacho_fruta_korea.srw
forward
global type w_info_despacho_fruta_korea from w_para_informes
end type
type st_4 from statictext within w_info_despacho_fruta_korea
end type
type st_1 from statictext within w_info_despacho_fruta_korea
end type
type st_6 from statictext within w_info_despacho_fruta_korea
end type
type st_2 from statictext within w_info_despacho_fruta_korea
end type
type em_planilla from editmask within w_info_despacho_fruta_korea
end type
type em_fecha_des from editmask within w_info_despacho_fruta_korea
end type
type st_11 from statictext within w_info_despacho_fruta_korea
end type
type ddlb_1 from dropdownlistbox within w_info_despacho_fruta_korea
end type
type rb_inspeccionado from radiobutton within w_info_despacho_fruta_korea
end type
type rb_tratado from radiobutton within w_info_despacho_fruta_korea
end type
type rb_tratainpe from radiobutton within w_info_despacho_fruta_korea
end type
type uo_selplanta from uo_seleccion_plantas within w_info_despacho_fruta_korea
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_despacho_fruta_korea
end type
end forward

global type w_info_despacho_fruta_korea from w_para_informes
integer x = 14
integer y = 32
integer width = 2711
integer height = 1376
string title = "Despacho de Fruta Inspeccionada S.A.G."
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_6 st_6
st_2 st_2
em_planilla em_planilla
em_fecha_des em_fecha_des
st_11 st_11
ddlb_1 ddlb_1
rb_inspeccionado rb_inspeccionado
rb_tratado rb_tratado
rb_tratainpe rb_tratainpe
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
end type
global w_info_despacho_fruta_korea w_info_despacho_fruta_korea

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo, ii_var, ii_cli
String	is_report, is_tipoplanilla

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta


end variables

forward prototypes
public function boolean existeplanilla (long al_planilla)
end prototypes

public function boolean existeplanilla (long al_planilla);Boolean	lb_Retorno
Date		ld_fecha

SELECT Min(defe_fecdes)
	INTO	:ld_fecha
	FROM	dbo.DESPAFRIGOEN 
	WHERE	plde_codigo =	:uo_SelPlanta.Codigo
	AND	clie_codigo	=	:uo_SelCliente.Codigo
	AND   defe_nturno =  :is_tipoplanilla
	AND	defe_plasag	=	:al_planilla;
			
If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Despachos")
	lb_Retorno = False
Else
	em_fecha_des.text	= String(ld_fecha)
	pb_acepta.Enabled	= True	
	lb_Retorno = True
End If

Return lb_Retorno
end function

on w_info_despacho_fruta_korea.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_6=create st_6
this.st_2=create st_2
this.em_planilla=create em_planilla
this.em_fecha_des=create em_fecha_des
this.st_11=create st_11
this.ddlb_1=create ddlb_1
this.rb_inspeccionado=create rb_inspeccionado
this.rb_tratado=create rb_tratado
this.rb_tratainpe=create rb_tratainpe
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_planilla
this.Control[iCurrent+6]=this.em_fecha_des
this.Control[iCurrent+7]=this.st_11
this.Control[iCurrent+8]=this.ddlb_1
this.Control[iCurrent+9]=this.rb_inspeccionado
this.Control[iCurrent+10]=this.rb_tratado
this.Control[iCurrent+11]=this.rb_tratainpe
this.Control[iCurrent+12]=this.uo_selplanta
this.Control[iCurrent+13]=this.uo_selcliente
end on

on w_info_despacho_fruta_korea.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_6)
destroy(this.st_2)
destroy(this.em_planilla)
destroy(this.em_fecha_des)
destroy(this.st_11)
destroy(this.ddlb_1)
destroy(this.rb_inspeccionado)
destroy(this.rb_tratado)
destroy(this.rb_tratainpe)
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	
	uo_SelCliente.dw_Seleccion.Object.Codigo[1] = gi_CodExport
	uo_SelCliente.Codigo = gi_CodExport
	
	uo_SelPlanta.dw_Seleccion.Object.Codigo[1] = gi_CodPlanta
	uo_SelPlanta.Codigo = gi_CodPlanta

	ddlb_1.SelectItem(Integer(1))	
	is_tipoplanilla = '1'
End If
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_despacho_fruta_korea
end type

type st_computador from w_para_informes`st_computador within w_info_despacho_fruta_korea
end type

type st_usuario from w_para_informes`st_usuario within w_info_despacho_fruta_korea
end type

type st_temporada from w_para_informes`st_temporada within w_info_despacho_fruta_korea
end type

type p_logo from w_para_informes`p_logo within w_info_despacho_fruta_korea
integer y = 24
end type

type st_titulo from w_para_informes`st_titulo within w_info_despacho_fruta_korea
integer width = 2011
string text = "Despacho de Fruta Anexo S.A.G. KOREA"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_despacho_fruta_korea
integer x = 2354
integer y = 452
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	li_fila
Long		ll_planilla_sag

istr_info.titulo	= 'DESPACHO DE FRUTA ANEXO KOREA'

ll_planilla_sag	=	Long(em_planilla.Text)

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_anexo_korea" 
vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_SelPlanta.Codigo,ll_planilla_sag,is_tipoplanilla)
								  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_despacho_fruta_korea
integer x = 2354
integer y = 748
integer taborder = 60
end type

type st_4 from statictext within w_info_despacho_fruta_korea
integer x = 251
integer y = 444
integer width = 2011
integer height = 576
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

type st_1 from statictext within w_info_despacho_fruta_korea
integer x = 334
integer y = 604
integer width = 448
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_despacho_fruta_korea
integer x = 334
integer y = 512
integer width = 448
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_despacho_fruta_korea
integer x = 334
integer y = 696
integer width = 448
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
string text = "Planilla S.A.G."
boolean focusrectangle = false
end type

type em_planilla from editmask within w_info_despacho_fruta_korea
event getfocus pbm_ensetfocus
integer x = 773
integer y = 688
integer width = 443
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
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "~t/"
end type

event modified;If Not ExistePlanilla(Long(This.Text)) Then 
	This.Text = ''
End If
end event

type em_fecha_des from editmask within w_info_despacho_fruta_korea
integer x = 1390
integer y = 688
integer width = 402
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_11 from statictext within w_info_despacho_fruta_korea
integer x = 329
integer y = 820
integer width = 448
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
string text = "Tipo Planilla"
boolean focusrectangle = false
end type

type ddlb_1 from dropdownlistbox within w_info_despacho_fruta_korea
integer x = 773
integer y = 820
integer width = 1403
integer height = 400
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string item[] = {"1. Productos Agríc. de Export. Certificados","2. Productos Agr.Export. Cert. (USDA)","3. Fruta a ser Fumigada en U.S.A.","4. Fumigados","5. Fruta a ser Fumigada en México"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;
is_tipoplanilla	=	String(index)
em_planilla.Text = ''
em_planilla.SetFocus()

end event

type rb_inspeccionado from radiobutton within w_info_despacho_fruta_korea
boolean visible = false
integer x = 507
integer y = 2360
integer width = 613
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "INSPECCIONADO"
boolean checked = true
end type

type rb_tratado from radiobutton within w_info_despacho_fruta_korea
boolean visible = false
integer x = 1266
integer y = 2360
integer width = 462
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "TRATADO"
end type

type rb_tratainpe from radiobutton within w_info_despacho_fruta_korea
boolean visible = false
integer x = 1810
integer y = 2360
integer width = 983
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "TRATADO E INSPECCIONADO"
end type

type uo_selplanta from uo_seleccion_plantas within w_info_despacho_fruta_korea
event destroy ( )
integer x = 773
integer y = 600
integer height = 88
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_info_despacho_fruta_korea
integer x = 773
integer y = 508
integer height = 88
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

