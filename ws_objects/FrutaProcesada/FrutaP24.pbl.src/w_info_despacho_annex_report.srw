$PBExportHeader$w_info_despacho_annex_report.srw
forward
global type w_info_despacho_annex_report from w_para_informes
end type
type st_4 from statictext within w_info_despacho_annex_report
end type
type st_1 from statictext within w_info_despacho_annex_report
end type
type dw_2 from datawindow within w_info_despacho_annex_report
end type
type st_6 from statictext within w_info_despacho_annex_report
end type
type dw_1 from datawindow within w_info_despacho_annex_report
end type
type st_2 from statictext within w_info_despacho_annex_report
end type
type em_planilla from editmask within w_info_despacho_annex_report
end type
type em_fecha_des from editmask within w_info_despacho_annex_report
end type
type cbx_packing from checkbox within w_info_despacho_annex_report
end type
type st_8 from statictext within w_info_despacho_annex_report
end type
type st_13 from statictext within w_info_despacho_annex_report
end type
type em_nomdespachador from editmask within w_info_despacho_annex_report
end type
type st_10 from statictext within w_info_despacho_annex_report
end type
type cbx_prdrot from checkbox within w_info_despacho_annex_report
end type
type st_11 from statictext within w_info_despacho_annex_report
end type
type rb_1 from radiobutton within w_info_despacho_annex_report
end type
type rb_2 from radiobutton within w_info_despacho_annex_report
end type
type st_3 from statictext within w_info_despacho_annex_report
end type
type st_5 from statictext within w_info_despacho_annex_report
end type
type em_nroplanilla from editmask within w_info_despacho_annex_report
end type
end forward

global type w_info_despacho_annex_report from w_para_informes
integer x = 14
integer y = 32
integer width = 2981
integer height = 1800
string title = "Informe Anexos Manzanas a Taiwan"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
dw_2 dw_2
st_6 st_6
dw_1 dw_1
st_2 st_2
em_planilla em_planilla
em_fecha_des em_fecha_des
cbx_packing cbx_packing
st_8 st_8
st_13 st_13
em_nomdespachador em_nomdespachador
st_10 st_10
cbx_prdrot cbx_prdrot
st_11 st_11
rb_1 rb_1
rb_2 rb_2
st_3 st_3
st_5 st_5
em_nroplanilla em_nroplanilla
end type
global w_info_despacho_annex_report w_info_despacho_annex_report

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

public function boolean existeplanilla (long al_planilla);Integer	li_codexp, li_planta
Date		ld_fecha

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF al_planilla <> 0 OR li_planta = 0 THEN

	SELECT Min(defe_fecdes)
		INTO	:ld_fecha
		FROM	dbo.DESPAFRIGOEN 
		WHERE	plde_codigo =	:li_planta
		AND	clie_codigo	=	:li_codexp
		AND	defe_plasag	=	:al_planilla;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		em_planilla.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_acepta.Enabled	= False
		em_planilla.SetFocus()
		RETURN False
	ELSE
		em_fecha_des.text		= String(ld_fecha)
		pb_acepta.Enabled	= True
		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

on w_info_despacho_annex_report.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
this.st_2=create st_2
this.em_planilla=create em_planilla
this.em_fecha_des=create em_fecha_des
this.cbx_packing=create cbx_packing
this.st_8=create st_8
this.st_13=create st_13
this.em_nomdespachador=create em_nomdespachador
this.st_10=create st_10
this.cbx_prdrot=create cbx_prdrot
this.st_11=create st_11
this.rb_1=create rb_1
this.rb_2=create rb_2
this.st_3=create st_3
this.st_5=create st_5
this.em_nroplanilla=create em_nroplanilla
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_2
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.dw_1
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.em_planilla
this.Control[iCurrent+8]=this.em_fecha_des
this.Control[iCurrent+9]=this.cbx_packing
this.Control[iCurrent+10]=this.st_8
this.Control[iCurrent+11]=this.st_13
this.Control[iCurrent+12]=this.em_nomdespachador
this.Control[iCurrent+13]=this.st_10
this.Control[iCurrent+14]=this.cbx_prdrot
this.Control[iCurrent+15]=this.st_11
this.Control[iCurrent+16]=this.rb_1
this.Control[iCurrent+17]=this.rb_2
this.Control[iCurrent+18]=this.st_3
this.Control[iCurrent+19]=this.st_5
this.Control[iCurrent+20]=this.em_nroplanilla
end on

on w_info_despacho_annex_report.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.st_2)
destroy(this.em_planilla)
destroy(this.em_fecha_des)
destroy(this.cbx_packing)
destroy(this.st_8)
destroy(this.st_13)
destroy(this.em_nomdespachador)
destroy(this.st_10)
destroy(this.cbx_prdrot)
destroy(this.st_11)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.em_nroplanilla)
end on

event open;x=0
y=0
dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1,"clie_codigo", gi_codexport)

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_1.InsertRow(0)
dw_1.SetItem(1,"plde_codigo", gi_codplanta)

IF gi_prod_rotulado = 1 THEN
	cbx_prdrot.Checked	= True
	cbx_prdrot.Enabled	= False
ELSE
	cbx_prdrot.Checked	= False
	cbx_prdrot.Enabled	= True
END IF	

IF gi_pack_rotulado = 1 THEN
	cbx_packing.Checked	= True
	cbx_packing.Enabled	= False
ELSE
	cbx_packing.Checked	= False
	cbx_packing.Enabled	= True
END IF	

istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)






	







end event

type pb_excel from w_para_informes`pb_excel within w_info_despacho_annex_report
end type

type st_computador from w_para_informes`st_computador within w_info_despacho_annex_report
end type

type st_usuario from w_para_informes`st_usuario within w_info_despacho_annex_report
end type

type st_temporada from w_para_informes`st_temporada within w_info_despacho_annex_report
end type

type p_logo from w_para_informes`p_logo within w_info_despacho_annex_report
end type

type st_titulo from w_para_informes`st_titulo within w_info_despacho_annex_report
integer width = 2222
string text = "Manzanas a Taiwan"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_despacho_annex_report
integer x = 2670
integer y = 972
integer taborder = 90
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	li_fila, li_planta, li_cliente, li_informe, li_prdrot, li_calrot, li_packing
Long		ll_planilla_sag


istr_info.titulo	= 'MANZANAS A TAIWAN'


li_cliente			=	Integer(istr_mant.argumento[1])
li_planta			=	Integer(istr_mant.argumento[2])
ll_planilla_sag	=	Long(em_planilla.Text)

OpenWithParm(vinf,istr_info)

IF rb_1.Checked	THEN
	vinf.dw_1.DataObject = "dw_info_annex_report" 
ELSE
	vinf.dw_1.DataObject = "dw_info_anexo_de_planilla"
END IF
	
IF cbx_prdrot.Checked THEN
	li_prdrot 	=	1
ELSE
	li_prdrot	=	0
END IF

IF cbx_packing.Checked THEN
	li_packing 	=	1
ELSE
	li_packing	=	0
END IF

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(li_cliente,li_planta,ll_planilla_sag,li_prdrot,li_packing)
								  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify("t_nomdespachador.text = '" + em_nomdespachador.text + "'")	
	vinf.dw_1.Modify("t_planilla.text = '" + em_nroplanilla.text + "'")	

	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_despacho_annex_report
integer x = 2665
integer y = 1252
integer taborder = 100
end type

type st_4 from statictext within w_info_despacho_annex_report
integer x = 251
integer y = 644
integer width = 2222
integer height = 340
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

type st_1 from statictext within w_info_despacho_annex_report
integer x = 311
integer y = 784
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

type dw_2 from datawindow within w_info_despacho_annex_report
integer x = 965
integer y = 668
integer width = 1161
integer height = 92
integer taborder = 30
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	String(data)
idwc_planta.Retrieve(1)
istr_mant.argumento[2]	=	String(dw_1.Object.plde_codigo[1])
dw_1.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))




	
end event

type st_6 from statictext within w_info_despacho_annex_report
integer x = 311
integer y = 692
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

type dw_1 from datawindow within w_info_despacho_annex_report
integer x = 965
integer y = 764
integer width = 983
integer height = 92
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]=String(data)
end event

type st_2 from statictext within w_info_despacho_annex_report
integer x = 311
integer y = 876
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

type em_planilla from editmask within w_info_despacho_annex_report
event getfocus pbm_ensetfocus
integer x = 965
integer y = 860
integer width = 443
integer height = 92
integer taborder = 50
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
string displaydata = "$"
end type

event modified;IF ExistePlanilla(Long(This.Text)) = False THEN
	This.SetFocus()
ELSE
	em_nroplanilla.Text = 'Nro.Planilla de Despacho ' + This.Text 		
END IF
end event

type em_fecha_des from editmask within w_info_despacho_annex_report
integer x = 1477
integer y = 860
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

type cbx_packing from checkbox within w_info_despacho_annex_report
integer x = 965
integer y = 548
integer width = 905
integer height = 80
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Packing Rotulado"
end type

event clicked;if this.checked = true then
	ii_cli = 1
else
	ii_cli = 0
end if	
end event

type st_8 from statictext within w_info_despacho_annex_report
integer x = 251
integer y = 440
integer width = 2222
integer height = 204
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

type st_13 from statictext within w_info_despacho_annex_report
integer x = 306
integer y = 1356
integer width = 658
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
string text = "Nombre Despachador "
boolean focusrectangle = false
end type

type em_nomdespachador from editmask within w_info_despacho_annex_report
event getfocus pbm_ensetfocus
integer x = 965
integer y = 1344
integer width = 1285
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type st_10 from statictext within w_info_despacho_annex_report
integer x = 251
integer y = 984
integer width = 2222
integer height = 180
integer taborder = 60
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

type cbx_prdrot from checkbox within w_info_despacho_annex_report
integer x = 965
integer y = 456
integer width = 850
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor Rotulado"
end type

type st_11 from statictext within w_info_despacho_annex_report
integer x = 306
integer y = 1040
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
string text = "Tipo Informe"
boolean focusrectangle = false
end type

type rb_1 from radiobutton within w_info_despacho_annex_report
integer x = 965
integer y = 996
integer width = 800
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
string text = "Phytosanitary Certificate"
boolean checked = true
boolean righttoleft = true
end type

type rb_2 from radiobutton within w_info_despacho_annex_report
integer x = 965
integer y = 1068
integer width = 978
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
string text = "Anexo a Planilla de Despacho"
boolean righttoleft = true
end type

type st_3 from statictext within w_info_despacho_annex_report
integer x = 251
integer y = 1164
integer width = 2222
integer height = 324
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

type st_5 from statictext within w_info_despacho_annex_report
integer x = 306
integer y = 1228
integer width = 658
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
string text = "Nro.Planilla"
boolean focusrectangle = false
end type

type em_nroplanilla from editmask within w_info_despacho_annex_report
event getfocus pbm_ensetfocus
integer x = 965
integer y = 1216
integer width = 1285
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

