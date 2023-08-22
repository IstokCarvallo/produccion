$PBExportHeader$w_info_fumigaenc_desc_lote.srw
$PBExportComments$Informe de Repalletizajes.
forward
global type w_info_fumigaenc_desc_lote from w_para_informes
end type
type st_4 from statictext within w_info_fumigaenc_desc_lote
end type
type st_1 from statictext within w_info_fumigaenc_desc_lote
end type
type st_5 from statictext within w_info_fumigaenc_desc_lote
end type
type st_2 from statictext within w_info_fumigaenc_desc_lote
end type
type em_numero from editmask within w_info_fumigaenc_desc_lote
end type
type dw_2 from datawindow within w_info_fumigaenc_desc_lote
end type
type st_6 from statictext within w_info_fumigaenc_desc_lote
end type
type dw_1 from datawindow within w_info_fumigaenc_desc_lote
end type
type cb_buscarepa from commandbutton within w_info_fumigaenc_desc_lote
end type
type st_3 from statictext within w_info_fumigaenc_desc_lote
end type
type dw_3 from datawindow within w_info_fumigaenc_desc_lote
end type
type st_9 from statictext within w_info_fumigaenc_desc_lote
end type
type st_7 from statictext within w_info_fumigaenc_desc_lote
end type
type st_8 from statictext within w_info_fumigaenc_desc_lote
end type
type st_10 from statictext within w_info_fumigaenc_desc_lote
end type
type cbx_n from checkbox within w_info_fumigaenc_desc_lote
end type
type cbx_pe from checkbox within w_info_fumigaenc_desc_lote
end type
type cbx_t from checkbox within w_info_fumigaenc_desc_lote
end type
type cbx_m from checkbox within w_info_fumigaenc_desc_lote
end type
type cbx_a from checkbox within w_info_fumigaenc_desc_lote
end type
type cbx_i from checkbox within w_info_fumigaenc_desc_lote
end type
type cbx_p from checkbox within w_info_fumigaenc_desc_lote
end type
type cbx_d from checkbox within w_info_fumigaenc_desc_lote
end type
type cbx_terceros from checkbox within w_info_fumigaenc_desc_lote
end type
type st_11 from statictext within w_info_fumigaenc_desc_lote
end type
end forward

global type w_info_fumigaenc_desc_lote from w_para_informes
integer x = 14
integer y = 32
integer width = 2706
integer height = 1680
string title = "INFORME DE CONDICIÓN"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
event type integer ue_guardar ( )
st_4 st_4
st_1 st_1
st_5 st_5
st_2 st_2
em_numero em_numero
dw_2 dw_2
st_6 st_6
dw_1 dw_1
cb_buscarepa cb_buscarepa
st_3 st_3
dw_3 dw_3
st_9 st_9
st_7 st_7
st_8 st_8
st_10 st_10
cbx_n cbx_n
cbx_pe cbx_pe
cbx_t cbx_t
cbx_m cbx_m
cbx_a cbx_a
cbx_i cbx_i
cbx_p cbx_p
cbx_d cbx_d
cbx_terceros cbx_terceros
st_11 st_11
end type
global w_info_fumigaenc_desc_lote w_info_fumigaenc_desc_lote

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_especie, idwc_condicion

uo_condicion    iuo_condicion      

end variables

forward prototypes
public function boolean noexistefolio (long al_numero)
end prototypes

event type integer ue_guardar();SetPointer(HourGlass!)

Long		ll_Filas, ll_fila1, ll_numero, ll_nueva, ll_nueva1, ll_fila, ll_nueva2,&
			ll_fila2, ll_nueva3, ll_fila3, fila_find, ll_pallet, fila_find2, ll_numerofumi
Integer	li_Cliente, li_planta,Respuesta
Boolean	lb_AutoCommit, lb_Retorno

IF em_numero.Text = "" THEN 
	MessageBox( "Advertencia", "Falta Número de Inspección.", &
					StopSign!, Ok!)
	RETURN 1
ELSE
	ll_numerofumi = Long(em_numero.Text)
END IF	


end event

public function boolean noexistefolio (long al_numero);Integer	li_Cliente, li_Planta, li_count
Long 		ll_numero

li_Cliente	=	Integer(istr_mant.argumento[3])
li_Planta	=	Integer(istr_mant.argumento[1])
ll_Numero	=	Long(em_numero.Text)

IF al_numero <> 0 THEN

	SELECT	count(*)
		INTO	:li_count
		FROM	dbo.fumigaenc
		WHERE	fumi_numsag	=	:al_numero
		AND	clie_codigo	=	:li_cliente
		AND	plde_codigo =	:li_planta;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Fumigaenc")
		em_numero.SetFocus()
		RETURN False
	ELSEIF li_count = 0 THEN
		MessageBox("Atención", "No existe Nro. Fumigación Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_numero.SetFocus()
		em_numero.Text = '' 
		RETURN False
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF

RETURN False

end function

on w_info_fumigaenc_desc_lote.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_5=create st_5
this.st_2=create st_2
this.em_numero=create em_numero
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
this.cb_buscarepa=create cb_buscarepa
this.st_3=create st_3
this.dw_3=create dw_3
this.st_9=create st_9
this.st_7=create st_7
this.st_8=create st_8
this.st_10=create st_10
this.cbx_n=create cbx_n
this.cbx_pe=create cbx_pe
this.cbx_t=create cbx_t
this.cbx_m=create cbx_m
this.cbx_a=create cbx_a
this.cbx_i=create cbx_i
this.cbx_p=create cbx_p
this.cbx_d=create cbx_d
this.cbx_terceros=create cbx_terceros
this.st_11=create st_11
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.dw_2
this.Control[iCurrent+7]=this.st_6
this.Control[iCurrent+8]=this.dw_1
this.Control[iCurrent+9]=this.cb_buscarepa
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.dw_3
this.Control[iCurrent+12]=this.st_9
this.Control[iCurrent+13]=this.st_7
this.Control[iCurrent+14]=this.st_8
this.Control[iCurrent+15]=this.st_10
this.Control[iCurrent+16]=this.cbx_n
this.Control[iCurrent+17]=this.cbx_pe
this.Control[iCurrent+18]=this.cbx_t
this.Control[iCurrent+19]=this.cbx_m
this.Control[iCurrent+20]=this.cbx_a
this.Control[iCurrent+21]=this.cbx_i
this.Control[iCurrent+22]=this.cbx_p
this.Control[iCurrent+23]=this.cbx_d
this.Control[iCurrent+24]=this.cbx_terceros
this.Control[iCurrent+25]=this.st_11
end on

on w_info_fumigaenc_desc_lote.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.cb_buscarepa)
destroy(this.st_3)
destroy(this.dw_3)
destroy(this.st_9)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.st_10)
destroy(this.cbx_n)
destroy(this.cbx_pe)
destroy(this.cbx_t)
destroy(this.cbx_m)
destroy(this.cbx_a)
destroy(this.cbx_i)
destroy(this.cbx_p)
destroy(this.cbx_d)
destroy(this.cbx_terceros)
destroy(this.st_11)
end on

event open;call super::open;String	ls_Planta

SELECT	plde_nombre
	INTO	:ls_Planta
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:gi_CodPlanta ;

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_CodExport)

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_1.InsertRow(0)
dw_1.SetItem(1, "plde_codigo", gi_CodPlanta)

dw_3.GetChild("cond_codigo", idwc_condicion)
idwc_condicion.SetTransObject(sqlca)
idwc_condicion.Retrieve()
dw_3.InsertRow(0)
dw_3.SetItem(1, "cond_codigo", 1)

istr_mant.argumento[1]	=	String(gi_codplanta)
istr_mant.argumento[2]	=	""//folio
istr_mant.argumento[3]	=	String(gi_codexport)
istr_mant.argumento[5]	=	ls_Planta
istr_mant.argumento[9]  =  '1'//condición




end event

type pb_excel from w_para_informes`pb_excel within w_info_fumigaenc_desc_lote
end type

type st_computador from w_para_informes`st_computador within w_info_fumigaenc_desc_lote
end type

type st_usuario from w_para_informes`st_usuario within w_info_fumigaenc_desc_lote
end type

type st_temporada from w_para_informes`st_temporada within w_info_fumigaenc_desc_lote
end type

type p_logo from w_para_informes`p_logo within w_info_fumigaenc_desc_lote
end type

type st_titulo from w_para_informes`st_titulo within w_info_fumigaenc_desc_lote
integer x = 261
integer y = 256
integer width = 1902
string text = "Informe Planilla de Descripción del Lote"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_fumigaenc_desc_lote
string tag = "Imprimir Reporte"
integer x = 2322
integer y = 880
integer taborder = 60
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta,li_especie,li_numero,li_varrot, Respuesta , li_prdrot, li_calrot, li_packrot
Long		li_planta,li_cliente

istr_info.titulo	= 'FUMIGACION DE PALLET'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_fumigacion_desc_lote"

IF em_numero.text ="" THEN
	istr_mant.argumento[2] = '-1'
END IF
vinf.dw_1.SetTransObject(sqlca)

	fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
										Long(istr_mant.Argumento[2]), &
										Integer(istr_mant.Argumento[3]),&
										Integer(istr_mant.argumento[9]))
	
	IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
		ELSE
							
			F_Membrete(vinf.dw_1)
			
			IF cbx_n.Checked THEN
				vinf.dw_1.Modify("t_n.text = '" + 'X' + "'")
			END IF
			
			IF cbx_pe.Checked THEN
				vinf.dw_1.Modify("t_pe.text = '" + 'X' + "'")
			END IF
			
			IF cbx_t.Checked THEN
				vinf.dw_1.Modify("t_t.text = '" + 'X' + "'")
			END IF
			
			IF cbx_m.Checked THEN
				vinf.dw_1.Modify("t_m.text = '" + 'X' + "'")
			END IF
			
			IF cbx_d.Checked THEN
				vinf.dw_1.Modify("t_d.text = '" + 'X' + "'")
			END IF
			
			IF cbx_a.Checked THEN
				vinf.dw_1.Modify("t_a.text = '" + 'X' + "'")
			END IF
			
			IF cbx_i.Checked THEN
				vinf.dw_1.Modify("t_i.text = '" + 'X' + "'")
			END IF
			
			IF cbx_p.Checked THEN
				vinf.dw_1.Modify("t_p.text = '" + 'X' + "'")
			END IF
			
		IF gs_Ambiente <> 'Windows' THEN
			F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		END IF
	END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_fumigaenc_desc_lote
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2322
integer y = 1200
integer taborder = 80
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_fumigaenc_desc_lote
integer x = 256
integer y = 440
integer width = 1902
integer height = 356
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

type st_1 from statictext within w_info_fumigaenc_desc_lote
integer x = 352
integer y = 596
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

type st_5 from statictext within w_info_fumigaenc_desc_lote
integer x = 256
integer y = 796
integer width = 1902
integer height = 148
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

type st_2 from statictext within w_info_fumigaenc_desc_lote
integer x = 361
integer y = 840
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
string text = "Nro Folio SAG"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_fumigaenc_desc_lote
integer x = 910
integer y = 820
integer width = 393
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF This.Text <> "" THEN
	IF NoExisteFolio(Long(This.Text)) THEN
		This.Text	=	""
		This.SetFocus()
	ELSE
		istr_mant.argumento[2]	=	String(Long(This.Text), '00000')
		
	END IF
END IF




end event

type dw_2 from datawindow within w_info_fumigaenc_desc_lote
integer x = 910
integer y = 480
integer width = 1161
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_planta	

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[3]	=	data
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_fumigaenc_desc_lote
integer x = 352
integer y = 488
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

type dw_1 from datawindow within w_info_fumigaenc_desc_lote
integer x = 910
integer y = 584
integer width = 969
integer height = 92
integer taborder = 30
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]

IF ExistePlanta(Integer(istr_mant.argumento[3]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	istr_mant.Argumento[5]	=	ls_Columna[1]
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cb_buscarepa from commandbutton within w_info_fumigaenc_desc_lote
integer x = 1349
integer y = 824
integer width = 91
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;istr_busq.argum[1]	=	istr_mant.argumento[3]
istr_busq.argum[2]	=	istr_mant.argumento[1]
istr_busq.argum[3]	=	String(dw_3.Object.cond_codigo[1])
istr_busq.argum[10] = ''

OpenWithParm(w_busc_fumigaenc, istr_busq)


istr_busq	       = Message.PowerObjectParm

IF istr_busq.argum[10] <> "" THEN
	em_numero.Text				= istr_busq.argum[10]
	istr_mant.argumento[2]	= istr_busq.argum[10]
ELSE
	em_numero.SetFocus()
END IF
end event

type st_3 from statictext within w_info_fumigaenc_desc_lote
integer x = 352
integer y = 696
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
string text = "Condición"
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_info_fumigaenc_desc_lote
integer x = 910
integer y = 688
integer width = 983
integer height = 92
integer taborder = 40
boolean bringtotop = true
string dataobject = "dddw_condicion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_Null

SetNull(li_Null)
iuo_condicion  = Create uo_condicion

IF iuo_condicion.Existe(Integer(Data),True,SqlCa) THEN
   istr_mant.argumento[9] = Data

ELSE
	This.SetItem(1,"cond_codigo",li_Null)

	RETURN 1
END IF


end event

event itemerror;RETURN 1
end event

type st_9 from statictext within w_info_fumigaenc_desc_lote
integer x = 256
integer y = 944
integer width = 1902
integer height = 372
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_fumigaenc_desc_lote
integer x = 361
integer y = 980
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
string text = "Tipo Muestreo"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_fumigaenc_desc_lote
integer x = 361
integer y = 1092
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
string text = "Tipo Selección"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_fumigaenc_desc_lote
integer x = 361
integer y = 1204
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
string text = "Tipo Despacho"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_n from checkbox within w_info_fumigaenc_desc_lote
integer x = 992
integer y = 972
integer width = 155
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
string text = "N"
boolean lefttext = true
end type

type cbx_pe from checkbox within w_info_fumigaenc_desc_lote
integer x = 1394
integer y = 972
integer width = 155
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
string text = "PE"
boolean lefttext = true
end type

type cbx_t from checkbox within w_info_fumigaenc_desc_lote
integer x = 992
integer y = 1084
integer width = 155
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
string text = "T"
boolean lefttext = true
end type

type cbx_m from checkbox within w_info_fumigaenc_desc_lote
integer x = 1394
integer y = 1084
integer width = 155
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
string text = "M"
boolean lefttext = true
end type

type cbx_a from checkbox within w_info_fumigaenc_desc_lote
integer x = 1797
integer y = 1084
integer width = 155
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
string text = "A"
boolean lefttext = true
end type

type cbx_i from checkbox within w_info_fumigaenc_desc_lote
integer x = 992
integer y = 1196
integer width = 155
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
string text = "I"
boolean lefttext = true
end type

type cbx_p from checkbox within w_info_fumigaenc_desc_lote
integer x = 1394
integer y = 1196
integer width = 155
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
string text = "P"
boolean lefttext = true
end type

type cbx_d from checkbox within w_info_fumigaenc_desc_lote
integer x = 1797
integer y = 1196
integer width = 155
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
string text = "D"
boolean lefttext = true
end type

type cbx_terceros from checkbox within w_info_fumigaenc_desc_lote
boolean visible = false
integer x = 626
integer y = 1356
integer width = 1253
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Incluye Terceros mismas Características"
end type

event clicked;IF This.Checked THEN
	dw_2.Enabled		=	False
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[3]	=	'-1'
ELSE
	dw_2.Enabled		=	True
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(255, 255, 255)
	istr_mant.argumento[3]	=	String(dw_2.Object.clie_codigo[1])
END IF
end event

type st_11 from statictext within w_info_fumigaenc_desc_lote
boolean visible = false
integer x = 256
integer y = 1316
integer width = 1902
integer height = 196
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

