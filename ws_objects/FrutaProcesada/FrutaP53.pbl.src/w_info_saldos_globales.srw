$PBExportHeader$w_info_saldos_globales.srw
forward
global type w_info_saldos_globales from w_para_informes
end type
type dw_etiqueta from datawindow within w_info_saldos_globales
end type
type dw_status from datawindow within w_info_saldos_globales
end type
type sle_vari_hasta from singlelineedit within w_info_saldos_globales
end type
type sle_vari_desde from singlelineedit within w_info_saldos_globales
end type
type cb_1 from commandbutton within w_info_saldos_globales
end type
type cb_busc_var_d from commandbutton within w_info_saldos_globales
end type
type em_vari_hasta from editmask within w_info_saldos_globales
end type
type em_vari_desde from editmask within w_info_saldos_globales
end type
type st_5 from statictext within w_info_saldos_globales
end type
type st_4 from statictext within w_info_saldos_globales
end type
type st_3 from statictext within w_info_saldos_globales
end type
type dw_especie from datawindow within w_info_saldos_globales
end type
type st_2 from statictext within w_info_saldos_globales
end type
type st_1 from statictext within w_info_saldos_globales
end type
type dw_prod_hasta from datawindow within w_info_saldos_globales
end type
type dw_prod_desde from datawindow within w_info_saldos_globales
end type
type gb_5 from groupbox within w_info_saldos_globales
end type
type gb_4 from groupbox within w_info_saldos_globales
end type
type gb_7 from groupbox within w_info_saldos_globales
end type
type gb_8 from groupbox within w_info_saldos_globales
end type
type gb_6 from groupbox within w_info_saldos_globales
end type
type cbx_todas from checkbox within w_info_saldos_globales
end type
type dw_planta from datawindow within w_info_saldos_globales
end type
type gb_3 from groupbox within w_info_saldos_globales
end type
type cbx_calibre from checkbox within w_info_saldos_globales
end type
type cbx_serie from checkbox within w_info_saldos_globales
end type
type cbx_etiqueta from checkbox within w_info_saldos_globales
end type
type cbx_status from checkbox within w_info_saldos_globales
end type
type cbx_categoria from checkbox within w_info_saldos_globales
end type
type cbx_envase from checkbox within w_info_saldos_globales
end type
type cbx_variedad from checkbox within w_info_saldos_globales
end type
type cbx_productor from checkbox within w_info_saldos_globales
end type
type cbx_prod from checkbox within w_info_saldos_globales
end type
type dw_vari from datawindow within w_info_saldos_globales
end type
type cbx_vari from checkbox within w_info_saldos_globales
end type
type dw_1 from datawindow within w_info_saldos_globales
end type
type gb_9 from groupbox within w_info_saldos_globales
end type
end forward

global type w_info_saldos_globales from w_para_informes
integer width = 3104
integer height = 1792
string title = "EXISTENCIAS GLOBALES"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 78748035
dw_etiqueta dw_etiqueta
dw_status dw_status
sle_vari_hasta sle_vari_hasta
sle_vari_desde sle_vari_desde
cb_1 cb_1
cb_busc_var_d cb_busc_var_d
em_vari_hasta em_vari_hasta
em_vari_desde em_vari_desde
st_5 st_5
st_4 st_4
st_3 st_3
dw_especie dw_especie
st_2 st_2
st_1 st_1
dw_prod_hasta dw_prod_hasta
dw_prod_desde dw_prod_desde
gb_5 gb_5
gb_4 gb_4
gb_7 gb_7
gb_8 gb_8
gb_6 gb_6
cbx_todas cbx_todas
dw_planta dw_planta
gb_3 gb_3
cbx_calibre cbx_calibre
cbx_serie cbx_serie
cbx_etiqueta cbx_etiqueta
cbx_status cbx_status
cbx_categoria cbx_categoria
cbx_envase cbx_envase
cbx_variedad cbx_variedad
cbx_productor cbx_productor
cbx_prod cbx_prod
dw_vari dw_vari
cbx_vari cbx_vari
dw_1 dw_1
gb_9 gb_9
end type
global w_info_saldos_globales w_info_saldos_globales

type variables
str_busqueda istr_busq
str_mant istr_mant

String          ls_nombre_obtenido
Integer        ii_espe_codigo_anterior

DataWindowChild 		idwc_variedades, idwc_clientes
end variables

forward prototypes
public function boolean wf_existe_variedad (integer li_clie_codigo, integer li_espe_codigo, integer li_vari_codigo)
public function integer wf_valida_campos_nulos ()
public function integer wf_verifica_varios ()
public function boolean wf_activa_pb_aceptar ()
public subroutine getchil ()
end prototypes

public function boolean wf_existe_variedad (integer li_clie_codigo, integer li_espe_codigo, integer li_vari_codigo);Boolean			lb_return


  SELECT "dba"."variedades"."vari_nombre"  
		    INTO :ls_nombre_obtenido
	 	    FROM "dba"."variedades"  
		   WHERE ( "dba"."variedades"."clie_codigo" =: li_clie_codigo ) AND  
      				 ( "dba"."variedades"."espe_codigo" =:li_espe_codigo  ) AND  
				      ( "dba"."variedades"."vari_codigo" =:li_vari_codigo )   ;
IF(IsNull(ls_nombre_obtenido))THEN
	MessageBox('ERROR','Variedad no existe para la Especie seleccionada')
	lb_return	= 	TRUE
ELSE
	lb_return	=	FALSE
ENd IF
Return lb_return
end function

public function integer wf_valida_campos_nulos ();Integer		li_return

 dw_prod_desde.AcceptText()
 dw_prod_hasta.AcceptText()

IF	IsNull(dw_prod_desde.GetItemNumber(1,1)) THEN
	MessageBox('ERROR','Productor inicial no puede ser nulo')
	li_return = 1
	RETURN li_return 
ELSE
	li_return = 0
END IF

IF IsNull(dw_prod_hasta.GetItemNumber(1,1)) THEN
	MessageBox('ERROR','Productor final no puede ser nulo')
	li_return = 1
	RETURN li_return 
ELSE
	li_return = 0
END IF

IF IsNull(dw_especie.GetItemNumber(1,1)) THEN
	MessageBox('ERROR','Especie final no puede ser nulo')
	li_return = 1
	RETURN li_return 
ELSE
	li_return = 0
END IF


end function

public function integer wf_verifica_varios ();Integer			li_setnull,li_return

SetNull(li_setnull)
dw_prod_desde.AcceptText()
dw_prod_hasta.AcceptText()

IF(Not(IsNull(dw_prod_desde.GetItemNumber(1,1))))AND(Not(IsNull(dw_prod_hasta.GetItemNumber(1,1))))THEN
	IF(dw_prod_desde.GetItemNumber(1,1) >	dw_prod_hasta.GetItemNumber(1,1))THEN
		MessageBox('ADVERTENCIA','Productor inicial no puede ser mayor que Productor final')
		dw_prod_desde.SetItem(1,1,li_setnull)
		li_return = 1
	ELSE
		li_return = 0
	END IF
END IF
IF(Not(IsNull(dw_prod_hasta.GetItemNumber(1,1))))AND(Not(IsNull(dw_prod_desde.GetItemNumber(1,1))))THEN
	IF(	dw_prod_hasta.GetItemNumber(1,1) < dw_prod_desde.GetItemNumber(1,1))THEN
		MessageBox('ADVERTENCIA','Productor final no puede ser menor que Productor inicial')
		dw_prod_hasta.SetItem(1,1,li_setnull)
		li_return = 1
	ELSE
		li_return = 0
	END IF
END IF


RETURN li_return
end function

public function boolean wf_activa_pb_aceptar ();Boolean		lb_return

dw_prod_desde.AcceptText()
dw_prod_hasta.AcceptText()
dw_especie.AcceptText()
dw_etiqueta.AcceptTExt()
IF	Not(IsNull(dw_prod_desde.GetItemNumber(1,1))) AND Not(IsNull(dw_prod_hasta.GetItemNumber(1,1))) AND Not(IsNull(dw_especie.GetItemNumber(1,'espe_codigo'))) AND  Not(IsNull(dw_etiqueta.GetItemNumber(1,'etiq_codigo')))  THEN
			
	lb_return = FALSE
ELSE
	lb_return = TRUE
END IF


RETURN	lb_return
end function

public subroutine getchil ();DataWindowChild 		dw_prod_desde_hija,dw_prod_hasta_hija,dw_planta_hija,dw_status_hija, &
							dw_etiqueta_hija,dw_especie_hija

dw_prod_desde.GetChild('prod_codigo', dw_prod_desde_hija)
dw_prod_desde_hija.SetTransObject(sqlca)
dw_prod_desde_hija.InsertRow(0)

dw_prod_desde.SetTransObject(sqlca)
dw_prod_desde.InsertRow(0)
dw_prod_desde_hija.Retrieve(gi_codexport)

dw_prod_hasta.GetChild('prod_codigo', dw_prod_hasta_hija)
dw_prod_hasta_hija.SetTransObject(sqlca)
dw_prod_hasta_hija.InsertRow(0)

dw_prod_hasta.SetTransObject(sqlca)
dw_prod_hasta.InsertRow(0)
dw_prod_hasta_hija.Retrieve(gi_codexport)


dw_especie.GetChild('espe_codigo', dw_especie_hija)
dw_especie_hija.SetTransObject(sqlca)
dw_especie_hija.InsertRow(0)

dw_especie.SetTransObject(sqlca)
dw_especie.InsertRow(0)
dw_especie_hija.Retrieve(gi_codexport)

dw_planta.GetChild('plde_codigo', dw_planta_hija)
dw_planta_hija.SetTransObject(sqlca)
dw_planta_hija.InsertRow(0)

dw_planta.SetTransObject(sqlca)
dw_planta.InsertRow(0)
dw_planta_hija.Retrieve(gi_codexport)

dw_status.GetChild('stat_codigo',dw_status_hija)
dw_status_hija.SetTransObject(sqlca)
dw_status_hija.InsertRow(0)

dw_status.SetTransObject(sqlca)
dw_status.InsertRow(0)
dw_status_hija.Retrieve()

//dw_etiqueta.GetChild('etiq_codigo',dw_etiqueta_hija)
//dw_etiqueta_hija.SetTransObject(sqlca)
//dw_etiqueta_hija.InsertRow(0)
//
//dw_etiqueta.SetTransObject(sqlca)
//dw_etiqueta.InsertRow(0)
//dw_etiqueta_hija.Retrieve(gi_codexport)
//
Commit ;
end subroutine

on w_info_saldos_globales.create
int iCurrent
call super::create
this.dw_etiqueta=create dw_etiqueta
this.dw_status=create dw_status
this.sle_vari_hasta=create sle_vari_hasta
this.sle_vari_desde=create sle_vari_desde
this.cb_1=create cb_1
this.cb_busc_var_d=create cb_busc_var_d
this.em_vari_hasta=create em_vari_hasta
this.em_vari_desde=create em_vari_desde
this.st_5=create st_5
this.st_4=create st_4
this.st_3=create st_3
this.dw_especie=create dw_especie
this.st_2=create st_2
this.st_1=create st_1
this.dw_prod_hasta=create dw_prod_hasta
this.dw_prod_desde=create dw_prod_desde
this.gb_5=create gb_5
this.gb_4=create gb_4
this.gb_7=create gb_7
this.gb_8=create gb_8
this.gb_6=create gb_6
this.cbx_todas=create cbx_todas
this.dw_planta=create dw_planta
this.gb_3=create gb_3
this.cbx_calibre=create cbx_calibre
this.cbx_serie=create cbx_serie
this.cbx_etiqueta=create cbx_etiqueta
this.cbx_status=create cbx_status
this.cbx_categoria=create cbx_categoria
this.cbx_envase=create cbx_envase
this.cbx_variedad=create cbx_variedad
this.cbx_productor=create cbx_productor
this.cbx_prod=create cbx_prod
this.dw_vari=create dw_vari
this.cbx_vari=create cbx_vari
this.dw_1=create dw_1
this.gb_9=create gb_9
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_etiqueta
this.Control[iCurrent+2]=this.dw_status
this.Control[iCurrent+3]=this.sle_vari_hasta
this.Control[iCurrent+4]=this.sle_vari_desde
this.Control[iCurrent+5]=this.cb_1
this.Control[iCurrent+6]=this.cb_busc_var_d
this.Control[iCurrent+7]=this.em_vari_hasta
this.Control[iCurrent+8]=this.em_vari_desde
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.st_3
this.Control[iCurrent+12]=this.dw_especie
this.Control[iCurrent+13]=this.st_2
this.Control[iCurrent+14]=this.st_1
this.Control[iCurrent+15]=this.dw_prod_hasta
this.Control[iCurrent+16]=this.dw_prod_desde
this.Control[iCurrent+17]=this.gb_5
this.Control[iCurrent+18]=this.gb_4
this.Control[iCurrent+19]=this.gb_7
this.Control[iCurrent+20]=this.gb_8
this.Control[iCurrent+21]=this.gb_6
this.Control[iCurrent+22]=this.cbx_todas
this.Control[iCurrent+23]=this.dw_planta
this.Control[iCurrent+24]=this.gb_3
this.Control[iCurrent+25]=this.cbx_calibre
this.Control[iCurrent+26]=this.cbx_serie
this.Control[iCurrent+27]=this.cbx_etiqueta
this.Control[iCurrent+28]=this.cbx_status
this.Control[iCurrent+29]=this.cbx_categoria
this.Control[iCurrent+30]=this.cbx_envase
this.Control[iCurrent+31]=this.cbx_variedad
this.Control[iCurrent+32]=this.cbx_productor
this.Control[iCurrent+33]=this.cbx_prod
this.Control[iCurrent+34]=this.dw_vari
this.Control[iCurrent+35]=this.cbx_vari
this.Control[iCurrent+36]=this.dw_1
this.Control[iCurrent+37]=this.gb_9
end on

on w_info_saldos_globales.destroy
call super::destroy
destroy(this.dw_etiqueta)
destroy(this.dw_status)
destroy(this.sle_vari_hasta)
destroy(this.sle_vari_desde)
destroy(this.cb_1)
destroy(this.cb_busc_var_d)
destroy(this.em_vari_hasta)
destroy(this.em_vari_desde)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.dw_especie)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.dw_prod_hasta)
destroy(this.dw_prod_desde)
destroy(this.gb_5)
destroy(this.gb_4)
destroy(this.gb_7)
destroy(this.gb_8)
destroy(this.gb_6)
destroy(this.cbx_todas)
destroy(this.dw_planta)
destroy(this.gb_3)
destroy(this.cbx_calibre)
destroy(this.cbx_serie)
destroy(this.cbx_etiqueta)
destroy(this.cbx_status)
destroy(this.cbx_categoria)
destroy(this.cbx_envase)
destroy(this.cbx_variedad)
destroy(this.cbx_productor)
destroy(this.cbx_prod)
destroy(this.dw_vari)
destroy(this.cbx_vari)
destroy(this.dw_1)
destroy(this.gb_9)
end on

event open;call super::open;getchil()

dw_1.GetChild("clie_codigo", idwc_clientes)
idwc_clientes.SetTransObject(SQLCA)
idwc_clientes.Retrieve()
dw_1.InsertRow(0)
dw_1.SetItem(1, "clie_codigo", gi_CodExport)

dw_vari.GetChild('vari_codigo', idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
idwc_variedades.InsertRow(0)

dw_vari.SetTransObject(sqlca)
dw_vari.InsertRow(0)
idwc_variedades.Retrieve(gi_CodExport, gi_CodEspecie)

istr_mant.argumento[4]	=	String(gi_CodExport)
end event

type st_computador from w_para_informes`st_computador within w_info_saldos_globales
end type

type st_usuario from w_para_informes`st_usuario within w_info_saldos_globales
end type

type st_temporada from w_para_informes`st_temporada within w_info_saldos_globales
end type

type p_logo from w_para_informes`p_logo within w_info_saldos_globales
end type

type st_titulo from w_para_informes`st_titulo within w_info_saldos_globales
integer x = 535
integer y = 52
long backcolor = 78748035
string text = "Informe de  Existencias Globales Fruta Procesada"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_saldos_globales
integer x = 2670
integer y = 572
integer taborder = 170
end type

event pb_acepta::clicked;Integer	fila , especies ,status,li_variedad
String	l_s_titulo
Long		ll_productor,ll_planta,li_cliente,ll_variedad


l_s_titulo	= 'SALDOS GLOBALES'
istr_info.titulo	= 'Movimiento de Fruta Procesada '+l_s_titulo 

li_cliente 		=Integer(istr_mant.argumento[4])

IF cbx_prod.Checked THEN
	ll_productor=0
ELSE
	ll_productor=dw_prod_desde.GetITemNumber(1,'prod_codigo')
END IF

IF cbx_todas.Checked THEN
	ll_planta=0
ELSE
	ll_planta=dw_planta.GetITemNumber(1,'plde_codigo')
END IF

IF cbx_vari.Checked THEN
	li_variedad=0
ELSE
	li_variedad=dw_vari.GetITemNumber(1,'vari_codigo')
END IF

status 	=	dw_status.GetITemNumber(dw_status.GetRow(),'stat_codigo')
especies =	dw_especie.GetItemNumber(dw_especie.GetRow(),'espe_codigo')

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_despachos_globales_fp"
vinf.dw_1.SetTransObject(sqlca)



fila = vinf.dw_1.Retrieve(especies,status,li_variedad,ll_planta,ll_productor,li_cliente)

IF cbx_productor.Checked THEN
	vinf.dw_1.SetSort('vari_codigo,palletfruta.prod_codigo')	
END IF
IF cbx_variedad.Checked THEN
	vinf.dw_1.SetSort('vari_codigo,espe_codigo,emba_codigo, prod_codigo')	
END IF
IF cbx_envase.Checked THEN
	vinf.dw_1.SetSort('vari_codigo,emba_codigo,espe_codigo,prod_codigo')	
END IF
IF cbx_categoria.Checked THEN
	vinf.dw_1.SetSort('vari_codigo,palletencab_cate_codigo,espe_codigo,emba_codigo, prod_codigo')	
END IF
IF cbx_status.Checked THEN
	vinf.dw_1.SetSort('palletencab_stat_codigo,palletencab_cate_codigo,vari_codigo,espe_codigo,emba_codigo, prod_codigo')	
END IF
IF cbx_etiqueta.Checked THEN
	vinf.dw_1.SetSort('palletencab_etiq_codigo,palletencab_stat_codigo,palletencab_cate_codigo,vari_codigo,espe_codigo,emba_codigo, prod_codigo')	
END IF
IF cbx_calibre.Checked THEN
	vinf.dw_1.SetSort('palletfruta_pafr_calibre,vari_codigo,palletencab_etiq_codigo,palletencab_stat_codigo,palletencab_cate_codigo,espe_codigo,emba_codigo, prod_codigo')	
END IF

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	StopSign!, Ok!)
ELSE
	vinf.dw_1.SorT()
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_saldos_globales
integer x = 2688
integer y = 868
integer taborder = 0
end type

type dw_etiqueta from datawindow within w_info_saldos_globales
boolean visible = false
integer x = 411
integer y = 1156
integer width = 987
integer height = 104
string dataobject = "dw_mues_etiqueta"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type dw_status from datawindow within w_info_saldos_globales
integer x = 453
integer y = 1356
integer width = 974
integer height = 92
integer taborder = 70
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type sle_vari_hasta from singlelineedit within w_info_saldos_globales
boolean visible = false
integer x = 782
integer y = 816
integer width = 882
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type sle_vari_desde from singlelineedit within w_info_saldos_globales
boolean visible = false
integer x = 763
integer y = 704
integer width = 882
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type cb_1 from commandbutton within w_info_saldos_globales
boolean visible = false
integer x = 640
integer y = 824
integer width = 87
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;str_busqueda	lstr_busq

lstr_busq.argum[1] = istr_mant.Argumento[4]
lstr_busq.argum[2] = String(dw_especie.GetItemNumber(1, "espe_codigo"))

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] <> "" THEN
	dw_especie.SetItem(1, "espe_codigo", Integer(lstr_busq.argum[2]))
	em_vari_hasta.Text	= lstr_busq.argum[4] 
	sle_vari_hasta.Text	= lstr_busq.argum[5]
ELSE
	em_vari_hasta.SetFocus()
END IF
end event

type cb_busc_var_d from commandbutton within w_info_saldos_globales
boolean visible = false
integer x = 640
integer y = 704
integer width = 87
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;str_busqueda	lstr_busq

lstr_busq.argum[1] = istr_mant.Argumento[4]
lstr_busq.argum[2] = String(dw_especie.GetItemNumber(1, "espe_codigo"))

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] <> "" THEN
	//dw_especie.SetItem(1, "espe_codigo", Integer(lstr_busq.argum[2]))
	em_vari_desde.Text	= lstr_busq.argum[4] 
	sle_vari_desde.Text	= lstr_busq.argum[5]
ELSE
	em_vari_desde.SetFocus()
END IF
end event

type em_vari_hasta from editmask within w_info_saldos_globales
boolean visible = false
integer x = 430
integer y = 816
integer width = 215
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
string mask = "####"
end type

event modified;Boolean lb_return	
IF(lb_return	=	wf_existe_variedad(gi_codexport,Integer(dw_especie.GetItemNumber(1,'espe_codigo')),Integer(em_vari_hasta.Text)))THEN
		sle_vari_hasta.Text = ls_nombre_obtenido
		RETURN
END IF

end event

type em_vari_desde from editmask within w_info_saldos_globales
boolean visible = false
integer x = 430
integer y = 700
integer width = 215
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
string mask = "####"
end type

event modified;Boolean lb_return	
IF(lb_return	=	wf_existe_variedad(gi_codexport,Integer(dw_especie.GetItemNumber(1,'espe_codigo')),Integer(em_vari_desde.Text)))THEN
	sle_vari_desde.Text = ls_nombre_obtenido
	RETURN
END IF

end event

type st_5 from statictext within w_info_saldos_globales
integer x = 137
integer y = 784
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_saldos_globales
boolean visible = false
integer x = 137
integer y = 712
integer width = 192
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
boolean enabled = false
string text = "Desde "
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_saldos_globales
integer x = 137
integer y = 1064
integer width = 311
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
boolean enabled = false
string text = "Variedades"
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_saldos_globales
integer x = 453
integer y = 772
integer width = 878
integer height = 92
integer taborder = 40
boolean bringtotop = true
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.Argumento[4])

CHOOSE CASE dwo.name
	CASE 'espe_codigo'
			em_vari_desde.Text	= ''
			em_vari_hasta.Text	= ''
			sle_vari_desde.Text	= ''	
			sle_vari_hasta.Text	= ''
			idwc_variedades.Retrieve(gi_codexport, Integer(data))
END CHOOSE
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_saldos_globales
boolean visible = false
integer x = 151
integer y = 328
integer width = 192
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
boolean enabled = false
string text = "Hasta "
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_saldos_globales
boolean visible = false
integer x = 155
integer y = 228
integer width = 192
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type dw_prod_hasta from datawindow within w_info_saldos_globales
boolean visible = false
integer x = 416
integer y = 316
integer width = 1221
integer height = 92
boolean bringtotop = true
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
wf_verifica_varios()
end event

event itemerror;RETURN 1
end event

type dw_prod_desde from datawindow within w_info_saldos_globales
integer x = 453
integer y = 540
integer width = 1111
integer height = 108
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
end type

event itemchanged;AcceptText()
wf_verifica_varios()
end event

event itemerror;RETURN 1
end event

type gb_5 from groupbox within w_info_saldos_globales
integer x = 73
integer y = 692
integer width = 1600
integer height = 528
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Variedades"
end type

type gb_4 from groupbox within w_info_saldos_globales
integer x = 73
integer y = 392
integer width = 1600
integer height = 276
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Productores"
end type

event other;Parent.TRiggerEvent('eu_mousemove')
end event

type gb_7 from groupbox within w_info_saldos_globales
integer x = 73
integer y = 1236
integer width = 1609
integer height = 292
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Status"
end type

type gb_8 from groupbox within w_info_saldos_globales
boolean visible = false
integer x = 69
integer y = 1092
integer width = 1618
integer height = 204
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 79741120
string text = "Etiqueta"
end type

type gb_6 from groupbox within w_info_saldos_globales
integer x = 1705
integer y = 160
integer width = 1239
integer height = 276
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Planta"
end type

type cbx_todas from checkbox within w_info_saldos_globales
integer x = 1810
integer y = 216
integer width = 544
integer height = 80
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Todas"
boolean checked = true
boolean lefttext = true
end type

event clicked;IF(This.Checked )THEN
	dw_planta.Enabled = False
ELSE
	dw_planta.Enabled = True
END IF
end event

type dw_planta from datawindow within w_info_saldos_globales
integer x = 1810
integer y = 316
integer width = 969
integer height = 92
integer taborder = 90
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type gb_3 from groupbox within w_info_saldos_globales
integer x = 1705
integer y = 460
integer width = 818
integer height = 840
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Agrupados"
end type

type cbx_calibre from checkbox within w_info_saldos_globales
boolean visible = false
integer x = 1797
integer y = 1140
integer width = 544
integer height = 92
integer taborder = 190
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Calibre"
boolean lefttext = true
end type

type cbx_serie from checkbox within w_info_saldos_globales
integer x = 1797
integer y = 1052
integer width = 544
integer height = 92
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Calibre"
boolean lefttext = true
end type

type cbx_etiqueta from checkbox within w_info_saldos_globales
integer x = 1797
integer y = 964
integer width = 544
integer height = 92
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Etiqueta"
boolean lefttext = true
end type

type cbx_status from checkbox within w_info_saldos_globales
integer x = 1797
integer y = 876
integer width = 544
integer height = 92
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Status"
boolean lefttext = true
end type

type cbx_categoria from checkbox within w_info_saldos_globales
integer x = 1797
integer y = 788
integer width = 544
integer height = 92
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Categoria"
boolean lefttext = true
end type

type cbx_envase from checkbox within w_info_saldos_globales
integer x = 1797
integer y = 700
integer width = 544
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Envase"
boolean lefttext = true
end type

type cbx_variedad from checkbox within w_info_saldos_globales
integer x = 1797
integer y = 612
integer width = 544
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Variedad"
boolean lefttext = true
end type

type cbx_productor from checkbox within w_info_saldos_globales
integer x = 1797
integer y = 524
integer width = 544
integer height = 92
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Productor"
boolean lefttext = true
end type

type cbx_prod from checkbox within w_info_saldos_globales
integer x = 453
integer y = 448
integer width = 544
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Todos"
boolean checked = true
boolean lefttext = true
end type

event clicked;IF(This.Checked )THEN
	dw_prod_desde.Enabled = False
ELSE
	dw_prod_desde.Enabled = True
END IF
end event

type dw_vari from datawindow within w_info_saldos_globales
integer x = 453
integer y = 1048
integer width = 882
integer height = 92
integer taborder = 60
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_variedades"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
CHOOSE CASE dwo.name
	CASE 'espe_codigo'
			em_vari_desde.Text	= ''
			em_vari_hasta.Text	= ''
			sle_vari_desde.Text	= ''	
			sle_vari_hasta.Text	= ''
END CHOOSE
end event

event itemerror;RETURN 1
end event

type cbx_vari from checkbox within w_info_saldos_globales
integer x = 453
integer y = 932
integer width = 544
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Todas"
boolean checked = true
boolean lefttext = true
end type

event clicked;IF(This.Checked )THEN
	dw_vari.Enabled = False
ELSE
	dw_vari.Enabled = True
END IF
end event

type dw_1 from datawindow within w_info_saldos_globales
integer x = 114
integer y = 240
integer width = 1225
integer height = 92
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[4]=data
end event

event itemerror;RETURN 1
end event

type gb_9 from groupbox within w_info_saldos_globales
integer x = 73
integer y = 160
integer width = 1600
integer height = 228
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Cliente"
end type

