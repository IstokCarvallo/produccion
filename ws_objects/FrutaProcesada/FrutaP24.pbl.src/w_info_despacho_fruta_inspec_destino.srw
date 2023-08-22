$PBExportHeader$w_info_despacho_fruta_inspec_destino.srw
forward
global type w_info_despacho_fruta_inspec_destino from w_para_informes
end type
type rb_fruta_inspec from radiobutton within w_info_despacho_fruta_inspec_destino
end type
type rb_fruta_inspec_pl from radiobutton within w_info_despacho_fruta_inspec_destino
end type
type st_4 from statictext within w_info_despacho_fruta_inspec_destino
end type
type st_1 from statictext within w_info_despacho_fruta_inspec_destino
end type
type dw_2 from datawindow within w_info_despacho_fruta_inspec_destino
end type
type st_6 from statictext within w_info_despacho_fruta_inspec_destino
end type
type dw_1 from datawindow within w_info_despacho_fruta_inspec_destino
end type
type em_fecha from editmask within w_info_despacho_fruta_inspec_destino
end type
type st_3 from statictext within w_info_despacho_fruta_inspec_destino
end type
type st_2 from statictext within w_info_despacho_fruta_inspec_destino
end type
type em_planilla from editmask within w_info_despacho_fruta_inspec_destino
end type
type em_fecha_des from editmask within w_info_despacho_fruta_inspec_destino
end type
type cbx_var from checkbox within w_info_despacho_fruta_inspec_destino
end type
type cbx_prod1 from checkbox within w_info_despacho_fruta_inspec_destino
end type
type cbx_prod2 from checkbox within w_info_despacho_fruta_inspec_destino
end type
type st_7 from statictext within w_info_despacho_fruta_inspec_destino
end type
type cbx_clie from checkbox within w_info_despacho_fruta_inspec_destino
end type
type st_8 from statictext within w_info_despacho_fruta_inspec_destino
end type
type gb_3 from groupbox within w_info_despacho_fruta_inspec_destino
end type
type st_5 from statictext within w_info_despacho_fruta_inspec_destino
end type
type rb_1 from radiobutton within w_info_despacho_fruta_inspec_destino
end type
type rb_2 from radiobutton within w_info_despacho_fruta_inspec_destino
end type
type gb_4 from groupbox within w_info_despacho_fruta_inspec_destino
end type
type st_9 from statictext within w_info_despacho_fruta_inspec_destino
end type
type rb_3 from radiobutton within w_info_despacho_fruta_inspec_destino
end type
type st_13 from statictext within w_info_despacho_fruta_inspec_destino
end type
type em_nomdespachador from editmask within w_info_despacho_fruta_inspec_destino
end type
type st_10 from statictext within w_info_despacho_fruta_inspec_destino
end type
type cbx_packing from checkbox within w_info_despacho_fruta_inspec_destino
end type
type cbx_formatosag from checkbox within w_info_despacho_fruta_inspec_destino
end type
type cbx_prdrot from checkbox within w_info_despacho_fruta_inspec_destino
end type
type cbx_calrot from checkbox within w_info_despacho_fruta_inspec_destino
end type
type st_11 from statictext within w_info_despacho_fruta_inspec_destino
end type
type ddlb_1 from dropdownlistbox within w_info_despacho_fruta_inspec_destino
end type
type rb_4 from radiobutton within w_info_despacho_fruta_inspec_destino
end type
end forward

global type w_info_despacho_fruta_inspec_destino from w_para_informes
integer x = 14
integer y = 32
integer width = 3237
integer height = 2336
string title = "Despacho de Fruta Inspeccionada S.A.G."
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
rb_fruta_inspec rb_fruta_inspec
rb_fruta_inspec_pl rb_fruta_inspec_pl
st_4 st_4
st_1 st_1
dw_2 dw_2
st_6 st_6
dw_1 dw_1
em_fecha em_fecha
st_3 st_3
st_2 st_2
em_planilla em_planilla
em_fecha_des em_fecha_des
cbx_var cbx_var
cbx_prod1 cbx_prod1
cbx_prod2 cbx_prod2
st_7 st_7
cbx_clie cbx_clie
st_8 st_8
gb_3 gb_3
st_5 st_5
rb_1 rb_1
rb_2 rb_2
gb_4 gb_4
st_9 st_9
rb_3 rb_3
st_13 st_13
em_nomdespachador em_nomdespachador
st_10 st_10
cbx_packing cbx_packing
cbx_formatosag cbx_formatosag
cbx_prdrot cbx_prdrot
cbx_calrot cbx_calrot
st_11 st_11
ddlb_1 ddlb_1
rb_4 rb_4
end type
global w_info_despacho_fruta_inspec_destino w_info_despacho_fruta_inspec_destino

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

public function boolean existeplanilla (long al_planilla);Integer	li_codexp, li_planta, li_cansel, li_tecnic
Date		ld_fecha
String	ls_numsel, ls_ubisel, ls_tratam, ls_contraparte

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF al_planilla <> 0 OR li_planta = 0 THEN

	SELECT Min(defe_fecdes)
		INTO	:ld_fecha
		FROM	dba.DESPAFRIGOEN 
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

		SELECT  defe_cansel, defe_numsel, defe_ubisel, defe_tratam, defe_tecnic
			INTO	:li_cansel,  :ls_numsel,  :ls_ubisel,  :ls_tratam,  :li_tecnic
			FROM	dba.DESPAFRIGOEN 
			WHERE	plde_codigo =	:li_planta
			AND	clie_codigo	=	:li_codexp
			AND	defe_plasag	=	:al_planilla
			AND   defe_nturno =  :is_tipoplanilla
			AND   defe_fecdes =  :ld_fecha;				
		
		SELECT tecn_nombre+' '+tecn_apepat+' '+tecn_apemat
		   INTO  :ls_contraparte
		   FROM dba.cargostecnicos
			WHERE tecn_codigo = :li_tecnic
			AND   plde_codigo = :li_planta;		
	
		em_fecha_des.text		= String(ld_fecha)
		pb_acepta.Enabled	= True
		
		
		em_nomdespachador.Text	=	ls_contraparte
		
		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

on w_info_despacho_fruta_inspec_destino.create
int iCurrent
call super::create
this.rb_fruta_inspec=create rb_fruta_inspec
this.rb_fruta_inspec_pl=create rb_fruta_inspec_pl
this.st_4=create st_4
this.st_1=create st_1
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
this.em_fecha=create em_fecha
this.st_3=create st_3
this.st_2=create st_2
this.em_planilla=create em_planilla
this.em_fecha_des=create em_fecha_des
this.cbx_var=create cbx_var
this.cbx_prod1=create cbx_prod1
this.cbx_prod2=create cbx_prod2
this.st_7=create st_7
this.cbx_clie=create cbx_clie
this.st_8=create st_8
this.gb_3=create gb_3
this.st_5=create st_5
this.rb_1=create rb_1
this.rb_2=create rb_2
this.gb_4=create gb_4
this.st_9=create st_9
this.rb_3=create rb_3
this.st_13=create st_13
this.em_nomdespachador=create em_nomdespachador
this.st_10=create st_10
this.cbx_packing=create cbx_packing
this.cbx_formatosag=create cbx_formatosag
this.cbx_prdrot=create cbx_prdrot
this.cbx_calrot=create cbx_calrot
this.st_11=create st_11
this.ddlb_1=create ddlb_1
this.rb_4=create rb_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_fruta_inspec
this.Control[iCurrent+2]=this.rb_fruta_inspec_pl
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.dw_2
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_1
this.Control[iCurrent+8]=this.em_fecha
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.em_planilla
this.Control[iCurrent+12]=this.em_fecha_des
this.Control[iCurrent+13]=this.cbx_var
this.Control[iCurrent+14]=this.cbx_prod1
this.Control[iCurrent+15]=this.cbx_prod2
this.Control[iCurrent+16]=this.st_7
this.Control[iCurrent+17]=this.cbx_clie
this.Control[iCurrent+18]=this.st_8
this.Control[iCurrent+19]=this.gb_3
this.Control[iCurrent+20]=this.st_5
this.Control[iCurrent+21]=this.rb_1
this.Control[iCurrent+22]=this.rb_2
this.Control[iCurrent+23]=this.gb_4
this.Control[iCurrent+24]=this.st_9
this.Control[iCurrent+25]=this.rb_3
this.Control[iCurrent+26]=this.st_13
this.Control[iCurrent+27]=this.em_nomdespachador
this.Control[iCurrent+28]=this.st_10
this.Control[iCurrent+29]=this.cbx_packing
this.Control[iCurrent+30]=this.cbx_formatosag
this.Control[iCurrent+31]=this.cbx_prdrot
this.Control[iCurrent+32]=this.cbx_calrot
this.Control[iCurrent+33]=this.st_11
this.Control[iCurrent+34]=this.ddlb_1
this.Control[iCurrent+35]=this.rb_4
end on

on w_info_despacho_fruta_inspec_destino.destroy
call super::destroy
destroy(this.rb_fruta_inspec)
destroy(this.rb_fruta_inspec_pl)
destroy(this.st_4)
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.em_fecha)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.em_planilla)
destroy(this.em_fecha_des)
destroy(this.cbx_var)
destroy(this.cbx_prod1)
destroy(this.cbx_prod2)
destroy(this.st_7)
destroy(this.cbx_clie)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.gb_4)
destroy(this.st_9)
destroy(this.rb_3)
destroy(this.st_13)
destroy(this.em_nomdespachador)
destroy(this.st_10)
destroy(this.cbx_packing)
destroy(this.cbx_formatosag)
destroy(this.cbx_prdrot)
destroy(this.cbx_calrot)
destroy(this.st_11)
destroy(this.ddlb_1)
destroy(this.rb_4)
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

istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)

rb_fruta_inspec.Checked	=	True

em_fecha.text	=	String(Today())

ddlb_1.SelectItem(Integer(1))

is_tipoplanilla = '1'

IF gi_vari_rotulada = 1 THEN
	cbx_var.Checked	= True
	cbx_var.Enabled	= False
ELSE
	cbx_var.Checked	= False
	cbx_var.Enabled	= True
END IF	

IF gi_prod_rotulado = 1 THEN
	cbx_prdrot.Checked	= True
	cbx_prdrot.Enabled	= False
ELSE
	cbx_prdrot.Checked	= False
	cbx_prdrot.Enabled	= True
END IF	

IF gi_cali_rotulado = 1 THEN
	cbx_calrot.Checked	= True
	cbx_calrot.Enabled	= False
ELSE
	cbx_calrot.Checked	= False
	cbx_calrot.Enabled	= True
END IF	

ii_var	= gi_vari_rotulada






end event

type st_computador from w_para_informes`st_computador within w_info_despacho_fruta_inspec_destino
end type

type st_usuario from w_para_informes`st_usuario within w_info_despacho_fruta_inspec_destino
end type

type st_temporada from w_para_informes`st_temporada within w_info_despacho_fruta_inspec_destino
end type

type p_logo from w_para_informes`p_logo within w_info_despacho_fruta_inspec_destino
end type

type st_titulo from w_para_informes`st_titulo within w_info_despacho_fruta_inspec_destino
integer width = 2441
string text = "Despacho de Fruta Inspeccionada S.A.G."
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_despacho_fruta_inspec_destino
integer x = 2862
integer y = 1528
integer taborder = 70
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	li_fila, li_planta, li_cliente, li_informe, li_prdrot, li_calrot
Long		ll_planilla_sag

IF rb_1.Checked THEN
	istr_info.titulo	= 'DESPACHO DE FRUTA INSPECCIONADA ENTRE PLANTAS ADSCRITAS'
ELSE
	istr_info.titulo	= 'DESPACHO DE FRUTA PARA SER FUMIGADA EN U.S.A.'
END IF

li_cliente			=	Integer(istr_mant.argumento[1])
li_planta			=	Integer(istr_mant.argumento[2])
ll_planilla_sag	=	Long(em_planilla.Text)

OpenWithParm(vinf,istr_info)

IF rb_fruta_inspec.Checked	THEN
	IF cbx_formatosag.Checked THEN
		vinf.dw_1.DataObject = "dw_info_desp_frutainspec_unisag" //
		li_informe	=	1
	ELSE
		IF cbx_prod1.checked THEN
			IF cbx_packing.Checked THEN
				vinf.dw_1.DataObject = "dw_info_desp_frutainspec_variproducpack"
				li_informe	=	0
			ELSE
				vinf.dw_1.DataObject = "dw_info_desp_frutainspec_variporproduc" //
				li_informe	=	0				
			END IF
		ELSE
			vinf.dw_1.DataObject = "dw_info_despacho_fruta_inspec_variedad"
			li_informe	=	0			
		END IF
	END IF
END IF
IF rb_fruta_inspec_pl.Checked THEN
	IF cbx_formatosag.Checked THEN
		vinf.dw_1.DataObject = "dw_info_despacho_fruta_unisag" //
		li_informe	=	2		
	ELSE
	
		IF cbx_prod2.checked THEN
			vinf.dw_1.DataObject = "dw_info_despacho_fruta_inspecporproduc"  //
			li_informe	=	0			
		ELSE
			vinf.dw_1.DataObject = "dw_info_despacho_fruta_inspeccionada"
			li_informe	=	0
		END IF
	END IF
END IF

IF cbx_prdrot.Checked THEN
	li_prdrot 	=	1
ELSE
	li_prdrot	=	0
END IF

IF cbx_calrot.Checked THEN
	li_calrot 	=	1
ELSE
	li_calrot	=	0
END IF

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(li_cliente,li_planta,ll_planilla_sag,ii_var,ii_cli,li_informe,li_prdrot,li_calrot,is_tipoplanilla)
								  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF rb_1.Checked THEN
	   vinf.dw_1.Modify("t_titulo.text = '" + rb_1.text + "'")	
   ELSEIF rb_2.Checked THEN
	   vinf.dw_1.Modify("t_titulo.text = '" + rb_2.text + "'")
   ELSEIF rb_4.Checked THEN
	   vinf.dw_1.Modify("t_titulo.text = '" + rb_4.text + "'")
   ELSE
	   vinf.dw_1.Modify("t_titulo.text = '" + rb_3.text + "'")		
   END IF
	vinf.dw_1.Modify("planilla.text = '" + em_planilla.text + "'")
	vinf.dw_1.Modify("fechaemi.text = '" + em_fecha.text + "'")
	vinf.dw_1.Modify("t_defe_fecdes.text = '" + em_fecha.text + "'")	
	vinf.dw_1.Modify("t_nomdespachador.text = '" + em_nomdespachador.text + "'")	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_despacho_fruta_inspec_destino
integer x = 2857
integer y = 1808
integer taborder = 90
end type

type rb_fruta_inspec from radiobutton within w_info_despacho_fruta_inspec_destino
integer x = 347
integer y = 540
integer width = 704
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Fruta Inspeccionada"
boolean checked = true
end type

event clicked;//rb_fruta_inspec.Checked	=	True
IF rb_fruta_inspec.Checked THEN
	cbx_prod1.Enabled = TRUE
	cbx_prod2.Enabled = FALSE
	cbx_prod2.Checked = FALSE
	rb_3.Checked = True
ELSE
	cbx_prod1.Enabled = FALSE
END IF
	
end event

type rb_fruta_inspec_pl from radiobutton within w_info_despacho_fruta_inspec_destino
integer x = 343
integer y = 620
integer width = 1385
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Fruta Inspeccionada entre Plantas Adscritas"
end type

event clicked;//rb_fruta_inspec_pl.Checked	=	True
IF rb_fruta_inspec_pl.Checked THEN
	cbx_prod2.Enabled = TRUE
	cbx_prod1.Enabled = FALSE
	cbx_prod1.Checked = FALSE
	rb_1.Checked = True
ELSE
	cbx_prod2.Enabled = FALSE
END IF
end event

type st_4 from statictext within w_info_despacho_fruta_inspec_destino
integer x = 251
integer y = 1492
integer width = 2441
integer height = 544
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_despacho_fruta_inspec_destino
integer x = 343
integer y = 1732
integer width = 448
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_info_despacho_fruta_inspec_destino
integer x = 859
integer y = 1616
integer width = 1161
integer height = 92
integer taborder = 40
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

type st_6 from statictext within w_info_despacho_fruta_inspec_destino
integer x = 343
integer y = 1640
integer width = 448
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_info_despacho_fruta_inspec_destino
integer x = 855
integer y = 1712
integer width = 983
integer height = 92
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]=String(data)
end event

type em_fecha from editmask within w_info_despacho_fruta_inspec_destino
integer x = 859
integer y = 1516
integer width = 402
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_3 from statictext within w_info_despacho_fruta_inspec_destino
integer x = 343
integer y = 1532
integer width = 448
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
string text = "Fecha Emisión"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_despacho_fruta_inspec_destino
integer x = 343
integer y = 1824
integer width = 448
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
string text = "Planilla S.A.G."
boolean focusrectangle = false
end type

type em_planilla from editmask within w_info_despacho_fruta_inspec_destino
event getfocus pbm_ensetfocus
integer x = 859
integer y = 1808
integer width = 443
integer height = 92
integer taborder = 60
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

event modified;IF ExistePlanilla(Long(This.Text)) = False THEN
	This.SetFocus()
END IF
end event

type em_fecha_des from editmask within w_info_despacho_fruta_inspec_destino
integer x = 1477
integer y = 1808
integer width = 402
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type cbx_var from checkbox within w_info_despacho_fruta_inspec_destino
integer x = 471
integer y = 1376
integer width = 539
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Variedad Rot."
boolean checked = true
end type

event clicked;if this.checked = true then
	ii_var = 1
else
	ii_var = 0
end if	
end event

type cbx_prod1 from checkbox within w_info_despacho_fruta_inspec_destino
integer x = 2112
integer y = 572
integer width = 503
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Por Productor"
boolean checked = true
end type

type cbx_prod2 from checkbox within w_info_despacho_fruta_inspec_destino
integer x = 2112
integer y = 652
integer width = 503
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Por Productor"
boolean checked = true
end type

type st_7 from statictext within w_info_despacho_fruta_inspec_destino
integer x = 251
integer y = 1268
integer width = 1056
integer height = 220
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_clie from checkbox within w_info_despacho_fruta_inspec_destino
integer x = 1550
integer y = 1376
integer width = 480
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente Rot."
end type

event clicked;if this.checked = true then
	ii_cli = 1
else
	ii_cli = 0
end if	
end event

type st_8 from statictext within w_info_despacho_fruta_inspec_destino
integer x = 1312
integer y = 1268
integer width = 1381
integer height = 220
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_despacho_fruta_inspec_destino
integer x = 293
integer y = 464
integer width = 2354
integer height = 348
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Despacho"
end type

type st_5 from statictext within w_info_despacho_fruta_inspec_destino
integer x = 251
integer y = 444
integer width = 2441
integer height = 408
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_1 from radiobutton within w_info_despacho_fruta_inspec_destino
integer x = 343
integer y = 924
integer width = 2121
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "DESPACHO DE FRUTA INSPECCIONADA ENTRE PLANTAS ADSCRITAS"
end type

event clicked;////rb_fruta_inspec.Checked	=	True
//IF rb_fruta_inspec.Checked THEN
//	cbx_prod1.Enabled = TRUE
//	cbx_prod2.Enabled = FALSE
//	cbx_prod2.Checked = FALSE
//ELSE
//	cbx_prod1.Enabled = FALSE
//END IF

cbx_packing.Enabled = FALSE
cbx_packing.Checked = FALSE

IF rb_fruta_inspec.Checked THEN
	MessageBox("Atención", "Titulo no Corresponde a Planilla.", &
						Exclamation!, Ok!)
END IF						
end event

type rb_2 from radiobutton within w_info_despacho_fruta_inspec_destino
integer x = 343
integer y = 988
integer width = 1669
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "DESPACHO DE FRUTA PARA SER FUMIGADA EN U.S.A."
end type

event clicked;////rb_fruta_inspec_pl.Checked	=	True
//IF rb_fruta_inspec_pl.Checked THEN
//	cbx_prod2.Enabled = TRUE
//	cbx_prod1.Enabled = FALSE
//	cbx_prod1.Checked = FALSE
//ELSE
//	cbx_prod2.Enabled = FALSE
//END IF

IF rb_fruta_inspec.Checked THEN
	cbx_packing.Enabled = TRUE
	cbx_packing.Checked = TRUE
ELSE
	cbx_packing.Enabled = FALSE
	cbx_packing.Checked = FALSE
END IF


MessageBox("Atención", "Titulo no Corresponde a Planilla.", &
						Exclamation!, Ok!)

end event

type gb_4 from groupbox within w_info_despacho_fruta_inspec_destino
integer x = 293
integer y = 868
integer width = 2354
integer height = 360
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Titulo"
end type

type st_9 from statictext within w_info_despacho_fruta_inspec_destino
integer x = 251
integer y = 856
integer width = 2441
integer height = 408
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_3 from radiobutton within w_info_despacho_fruta_inspec_destino
integer x = 343
integer y = 1052
integer width = 1778
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "DESPACHO DE FRUTA INSPECCIONADA"
boolean checked = true
end type

event clicked;////rb_fruta_inspec.Checked	=	True
//IF rb_fruta_inspec.Checked THEN
//	cbx_prod1.Enabled = TRUE
//	cbx_prod2.Enabled = FALSE
//	cbx_prod2.Checked = FALSE
//ELSE
//	cbx_prod1.Enabled = FALSE
//END IF

cbx_packing.Enabled = FALSE
cbx_packing.Checked = FALSE

IF rb_fruta_inspec_pl.Checked THEN
	MessageBox("Atención", "Titulo no Corresponde a Planilla.", &
						Exclamation!, Ok!)
END IF			

	
end event

type st_13 from statictext within w_info_despacho_fruta_inspec_destino
integer x = 343
integer y = 2072
integer width = 658
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
string text = "Nombre Despachador "
boolean focusrectangle = false
end type

type em_nomdespachador from editmask within w_info_despacho_fruta_inspec_destino
event getfocus pbm_ensetfocus
integer x = 1015
integer y = 2072
integer width = 1285
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type st_10 from statictext within w_info_despacho_fruta_inspec_destino
integer x = 251
integer y = 2036
integer width = 2441
integer height = 160
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_packing from checkbox within w_info_despacho_fruta_inspec_destino
integer x = 2126
integer y = 1020
integer width = 434
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean enabled = false
string text = "Por Packing"
end type

type cbx_formatosag from checkbox within w_info_despacho_fruta_inspec_destino
integer x = 1056
integer y = 712
integer width = 709
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Formato Único S.A.G."
boolean checked = true
end type

type cbx_prdrot from checkbox within w_info_despacho_fruta_inspec_destino
integer x = 1550
integer y = 1284
integer width = 850
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Productor Rotulado"
boolean checked = true
end type

type cbx_calrot from checkbox within w_info_despacho_fruta_inspec_destino
integer x = 471
integer y = 1284
integer width = 795
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Calibre Rotulado Repa  "
end type

type st_11 from statictext within w_info_despacho_fruta_inspec_destino
integer x = 343
integer y = 1928
integer width = 448
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
string text = "Tipo Planilla"
boolean focusrectangle = false
end type

type ddlb_1 from dropdownlistbox within w_info_despacho_fruta_inspec_destino
integer x = 859
integer y = 1916
integer width = 1403
integer height = 400
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string item[] = {"1. Productos Agríc. de Export. Certificados","2. Productos Agr.Export. Cert. (USDA)","3. Fruta a ser Fumigada en U.S.A.","4. Fumigados","5. Fruta a ser Fumigada en México"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;is_tipoplanilla	=	String(index)
end event

type rb_4 from radiobutton within w_info_despacho_fruta_inspec_destino
integer x = 343
integer y = 1116
integer width = 1746
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "DESPACHO DE FRUTA PARA SER FUMIGADA EN MEXICO"
end type

event clicked;////rb_fruta_inspec_pl.Checked	=	True
//IF rb_fruta_inspec_pl.Checked THEN
//	cbx_prod2.Enabled = TRUE
//	cbx_prod1.Enabled = FALSE
//	cbx_prod1.Checked = FALSE
//ELSE
//	cbx_prod2.Enabled = FALSE
//END IF

IF rb_fruta_inspec.Checked THEN
	cbx_packing.Enabled = TRUE
	cbx_packing.Checked = TRUE
ELSE
	cbx_packing.Enabled = FALSE
	cbx_packing.Checked = FALSE
END IF


MessageBox("Atención", "Titulo no Corresponde a Planilla.", &
						Exclamation!, Ok!)
	
end event

