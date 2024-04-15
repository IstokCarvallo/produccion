$PBExportHeader$w_mant_cajasprod_cambia_caracteristicas.srw
$PBExportComments$Ventana de Informe de Control Calidad de Daños y Defectos
forward
global type w_mant_cajasprod_cambia_caracteristicas from w_para_informes
end type
type gb_4 from groupbox within w_mant_cajasprod_cambia_caracteristicas
end type
type st_3 from statictext within w_mant_cajasprod_cambia_caracteristicas
end type
type st_4 from statictext within w_mant_cajasprod_cambia_caracteristicas
end type
type pb_1 from picturebutton within w_mant_cajasprod_cambia_caracteristicas
end type
type rb_2 from radiobutton within w_mant_cajasprod_cambia_caracteristicas
end type
type st_5 from statictext within w_mant_cajasprod_cambia_caracteristicas
end type
type st_2 from statictext within w_mant_cajasprod_cambia_caracteristicas
end type
type em_recepcion from editmask within w_mant_cajasprod_cambia_caracteristicas
end type
type dw_1 from datawindow within w_mant_cajasprod_cambia_caracteristicas
end type
type st_6 from statictext within w_mant_cajasprod_cambia_caracteristicas
end type
type uo_selmercado from uo_seleccion_mercados within w_mant_cajasprod_cambia_caracteristicas
end type
type st_7 from statictext within w_mant_cajasprod_cambia_caracteristicas
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_cajasprod_cambia_caracteristicas
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_cajasprod_cambia_caracteristicas
end type
type rb_1 from radiobutton within w_mant_cajasprod_cambia_caracteristicas
end type
type st_9 from statictext within w_mant_cajasprod_cambia_caracteristicas
end type
type em_emb from editmask within w_mant_cajasprod_cambia_caracteristicas
end type
type st_10 from statictext within w_mant_cajasprod_cambia_caracteristicas
end type
type st_11 from statictext within w_mant_cajasprod_cambia_caracteristicas
end type
type st_12 from statictext within w_mant_cajasprod_cambia_caracteristicas
end type
type em_cal from editmask within w_mant_cajasprod_cambia_caracteristicas
end type
type dw_cat from datawindow within w_mant_cajasprod_cambia_caracteristicas
end type
type dw_eti from datawindow within w_mant_cajasprod_cambia_caracteristicas
end type
type dw_2 from datawindow within w_mant_cajasprod_cambia_caracteristicas
end type
type st_8 from statictext within w_mant_cajasprod_cambia_caracteristicas
end type
type pb_nuevo from picturebutton within w_mant_cajasprod_cambia_caracteristicas
end type
type sle_camara from editmask within w_mant_cajasprod_cambia_caracteristicas
end type
type dw_lotes from datawindow within w_mant_cajasprod_cambia_caracteristicas
end type
end forward

global type w_mant_cajasprod_cambia_caracteristicas from w_para_informes
integer width = 4608
integer height = 2408
event ue_recuperadatos ( )
event ue_imprimir ( )
event ue_guardar ( )
event ue_antesguardar ( )
event ue_nuevo ( )
gb_4 gb_4
st_3 st_3
st_4 st_4
pb_1 pb_1
rb_2 rb_2
st_5 st_5
st_2 st_2
em_recepcion em_recepcion
dw_1 dw_1
st_6 st_6
uo_selmercado uo_selmercado
st_7 st_7
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
rb_1 rb_1
st_9 st_9
em_emb em_emb
st_10 st_10
st_11 st_11
st_12 st_12
em_cal em_cal
dw_cat dw_cat
dw_eti dw_eti
dw_2 dw_2
st_8 st_8
pb_nuevo pb_nuevo
sle_camara sle_camara
dw_lotes dw_lotes
end type
global w_mant_cajasprod_cambia_caracteristicas w_mant_cajasprod_cambia_caracteristicas

type variables
uo_correlcompequipo	iuo_Equipo
uo_embalajesprod		iuo_emba
uo_categorias			iuo_cate
uo_etiquetas			iuo_etiq
uo_calibresenvases	iuo_cali

str_busqueda	istr_busq
str_mant			istr_mant

Integer			ii_Parametro
Long				il_Inicio[], il_Final[]

DataWindowChild	idwc_productor
end variables

forward prototypes
public subroutine obtienesegmentos ()
public function boolean wf_actualiza_db ()
end prototypes

event ue_recuperadatos();Datawindowchild  ldwc_lotes
Long	ll_fila, respuesta

DO
	dw_2.Reset()
	
	dw_lotes.GetChild("vari_codigo", ldwc_lotes)
	ldwc_lotes.SetTransObject(SqlCa)
	ldwc_lotes.Retrieve(0)

	dw_lotes.SetTransObject(Sqlca)
	ll_fila	= dw_lotes.Retrieve(uo_SelPlanta.Codigo, long(em_recepcion.Text), uo_SelCliente.Codigo)

	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila = 0 THEN
		Messagebox("Error","No Existe Pallet Para Esta Planta")
	ELSE
		dw_2.SetTransObject(Sqlca)
		ll_fila	= dw_2.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, long(em_recepcion.Text))
		
		pb_acepta.Enabled 										= 	True
		em_emb.Enabled											=	True
		dw_cat.Enabled											=	True
		dw_eti.Enabled												=	True
		em_cal.Enabled											=	True
		
		dw_cat.Object.cate_codigo.BackGround.Color	=	RGB(255,255,255)
		dw_eti.Object.etiq_codigo.BackGround.Color	=	RGB(255,255,255)

	END IF									
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir();Long	ll_Fila

SetPointer(HourGlass!)

If Not iuo_Equipo.ObtieneEspecie(uo_SelCliente.Codigo, Long(em_recepcion.Text), True, Sqlca) Then Return
If Not iuo_Equipo.ObtieneFormato(uo_SelPlanta.Codigo, gstr_us.Computador, True, Sqlca) Then Return

dw_1.dataObject	=	iuo_Equipo.Formato
dw_1.SetTransObject(SQLCA)
dw_1.Reset()

For ll_fila = 1 TO dw_lotes.RowCount()
	IF dw_1.DataObject = "dw_info_spro_cajasprod_pomaceas" THEN
		dw_1.Retrieve(uo_SelCliente.Codigo,  &
						  uo_SelPlanta.Codigo,   &
						  dw_lotes.object.pafr_secuen[ll_fila], &
						  sle_camara.Text)

		FOR ll_Fila = 1 TO dw_1.RowCount()
			dw_1.Object.envo_descrip.visible		=	1
		NEXT
	
	ELSE
		dw_1.Retrieve(uo_SelCliente.Codigo,  &
						  uo_SelPlanta.Codigo, &
						  dw_lotes.object.pafr_secuen[ll_fila], &
						  dw_lotes.object.pafr_secuen[ll_fila], &
						  uo_SelMercado.Codigo)
	END IF
Next

IF dw_1.RowCount() > 0 THEN
	dw_1.Print(True, True)
END IF

dw_1.Reset()

SetPointer(Arrow!)

end event

event ue_guardar();IF dw_lotes.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_antesguardar();Long	ll_fila = 1

DO WHILE ll_fila <= dw_lotes.RowCount()
	IF dw_lotes.GetItemStatus(ll_fila, 0, Primary!) = New! THEN
		dw_lotes.DeleteRow(ll_fila)
	ELSE
		ll_fila ++
	END IF
LOOP
end event

event ue_nuevo();Integer	li_null

SetNull(li_null)

dw_lotes.reset()
dw_2.Reset()

em_cal.Text	=	String(li_null)
em_emb.Text	=	String(li_null)

dw_eti.Object.etiq_codigo[1]	=	li_null
dw_cat.Object.cate_codigo[1]	=	li_null

em_recepcion.text					=	String(li_null)

em_emb.Enabled					=	False
dw_cat.Enabled					=	False
dw_eti.Enabled						=	False
em_cal.Enabled					=	False

dw_cat.Object.cate_codigo.BackGround.Color	=	553648127
dw_eti.Object.etiq_codigo.BackGround.Color	=	553648127
end event

public subroutine obtienesegmentos ();Integer	li_filas, li_segmentos

li_segmentos = 1

FOR li_filas =  1 TO dw_lotes.RowCount()
	
	IF dw_Lotes.IsSelected(li_filas) THEN
		
		IF li_filas = 1 THEN 
			il_inicio[li_segmentos]	=	dw_lotes.Object.pafr_secuen[li_filas]
		ELSEIF li_filas = dw_lotes.RowCount() THEN 
			il_final[li_segmentos]	=	dw_lotes.Object.pafr_secuen[li_filas]
			li_segmentos ++
		ELSEIF dw_Lotes.IsSelected(li_filas - 1) = FALSE THEN
			il_inicio[li_segmentos]	=	dw_lotes.Object.pafr_secuen[li_filas]
		END IF
	
	ELSE
		
		IF li_filas > 1 THEN
			IF dw_Lotes.IsSelected(li_filas - 1) THEN
				il_final[li_segmentos]	=	dw_lotes.Object.pafr_secuen[li_filas - 1]
				li_segmentos ++
			END IF
		END IF
		
	END IF
	
NEXT
end subroutine

public function boolean wf_actualiza_db ();boolean			lb_Retorno, lb_AutoCommit
Integer			li_especie, li_etiqueta, li_categoria, li_planta, li_cliente, li_filas
Long				ll_caja, li_fila, ll_pallet
String			ls_embalaje, ls_Calibre


lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_planta			=	uo_SelPlanta.Codigo	
li_cliente			=	uo_SelCliente.Codigo

FOR li_fila = 1 TO dw_lotes.RowCount()
	li_etiqueta			=	dw_lotes.Object.etiq_codigo[li_fila]
	li_categoria			=	dw_lotes.Object.cate_codigo[li_fila]
	ls_embalaje			=	dw_lotes.Object.emba_codigo[li_fila]
	ls_Calibre			=	dw_lotes.Object.pafr_calibr[li_fila]
	ll_caja				=	dw_lotes.Object.pafr_secuen[li_fila]
	ll_pallet				=	Long(em_recepcion.text)
	
	DECLARE ModificaCajas PROCEDURE FOR dbo.Fgran_Modifica_Caract_Pallets   
		@Planta 		=	:li_planta,   
		@Cliente 	=	:li_cliente,   
		@pallet 		=	:ll_pallet,   
		@Numero 	=	:ll_caja,   
		@Categoria	=	:li_categoria,   
		@Etiqueta 	=	:li_etiqueta,
		@Embalaje	=	:ls_embalaje,
		@Calibre		=	:ls_Calibre
	using sqlca;
	
	Execute ModificaCajas;
	
	IF sqlca.SQLCode <> 100 THEN
		Rollback;
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla CajasProd")
		lb_Retorno	=	False
	ELSE
		Commit;
		
	END IF
NEXT

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

on w_mant_cajasprod_cambia_caracteristicas.create
int iCurrent
call super::create
this.gb_4=create gb_4
this.st_3=create st_3
this.st_4=create st_4
this.pb_1=create pb_1
this.rb_2=create rb_2
this.st_5=create st_5
this.st_2=create st_2
this.em_recepcion=create em_recepcion
this.dw_1=create dw_1
this.st_6=create st_6
this.uo_selmercado=create uo_selmercado
this.st_7=create st_7
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
this.rb_1=create rb_1
this.st_9=create st_9
this.em_emb=create em_emb
this.st_10=create st_10
this.st_11=create st_11
this.st_12=create st_12
this.em_cal=create em_cal
this.dw_cat=create dw_cat
this.dw_eti=create dw_eti
this.dw_2=create dw_2
this.st_8=create st_8
this.pb_nuevo=create pb_nuevo
this.sle_camara=create sle_camara
this.dw_lotes=create dw_lotes
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_4
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.pb_1
this.Control[iCurrent+5]=this.rb_2
this.Control[iCurrent+6]=this.st_5
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.em_recepcion
this.Control[iCurrent+9]=this.dw_1
this.Control[iCurrent+10]=this.st_6
this.Control[iCurrent+11]=this.uo_selmercado
this.Control[iCurrent+12]=this.st_7
this.Control[iCurrent+13]=this.uo_selplanta
this.Control[iCurrent+14]=this.uo_selcliente
this.Control[iCurrent+15]=this.rb_1
this.Control[iCurrent+16]=this.st_9
this.Control[iCurrent+17]=this.em_emb
this.Control[iCurrent+18]=this.st_10
this.Control[iCurrent+19]=this.st_11
this.Control[iCurrent+20]=this.st_12
this.Control[iCurrent+21]=this.em_cal
this.Control[iCurrent+22]=this.dw_cat
this.Control[iCurrent+23]=this.dw_eti
this.Control[iCurrent+24]=this.dw_2
this.Control[iCurrent+25]=this.st_8
this.Control[iCurrent+26]=this.pb_nuevo
this.Control[iCurrent+27]=this.sle_camara
this.Control[iCurrent+28]=this.dw_lotes
end on

on w_mant_cajasprod_cambia_caracteristicas.destroy
call super::destroy
destroy(this.gb_4)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.pb_1)
destroy(this.rb_2)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_recepcion)
destroy(this.dw_1)
destroy(this.st_6)
destroy(this.uo_selmercado)
destroy(this.st_7)
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
destroy(this.rb_1)
destroy(this.st_9)
destroy(this.em_emb)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.st_12)
destroy(this.em_cal)
destroy(this.dw_cat)
destroy(this.dw_eti)
destroy(this.dw_2)
destroy(this.st_8)
destroy(this.pb_nuevo)
destroy(this.sle_camara)
destroy(this.dw_lotes)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelMercado.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_Selplanta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE	
	uo_SelMercado.Seleccion(False,False)
	uo_SelPlanta.Seleccion(False,False)
	uo_SelCliente.Seleccion(False,False)
	
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	uo_SelCliente.Inicia(gi_CodExport)
	
	IF uo_SelCliente.Codigo = 81 THEN
		sle_camara.Visible		=	False
		st_7.Text					=	"Mercado"
		uo_selmercado.Visible	=	True
	ELSE
		sle_camara.Visible		=	True
		st_7.Text					=	"Camara"
		uo_selmercado.Visible	=	False
	END IF
	
	iuo_emba	=	Create uo_embalajesprod
	iuo_cate		=	Create uo_categorias			
	iuo_etiq		=	Create uo_etiquetas
	iuo_cali		=	Create uo_calibresenvases
	iuo_Equipo	=	Create uo_correlcompequipo
	
	dw_cat.SetTransObject(sqlca)
	dw_eti.SetTransObject(sqlca)
	
	dw_cat.InsertRow(0)
	dw_eti.InsertRow(0)
	
	dw_2.GetChild('prod_codrot', idwc_productor)
	idwc_productor.SetTransObject(SqlCa)
	idwc_productor.Retrieve(-1)
	
	ii_Parametro 	=	Integer(Message.StringParm)

	IF ii_Parametro = 3 THEN
		This.Title 						=	'MANTENCIÓN DE DETALLE DE PALLETS'
		st_titulo.Text					=	'MANTENCIÓN DE DETALLE DE PALLETS'
	END IF

	pb_acepta.Enabled 				=	False
	TriggerEvent("ue_nuevo")
END IF
end event

event resize;call super::resize;Integer		li_posi_y, li_objeto

pb_nuevo.x			=	pb_acepta.x
pb_nuevo.y			=	pb_acepta.y - 255

st_5.Height			=	This.WorkSpaceHeight() - st_5.y - 75

dw_2.Width			=	st_5.Width - 80
dw_2.Height			=	st_5.Height - 60

st_8.Width			=	st_2.Width
st_8.x					=	st_2.x
gb_4.Width			=	st_2.Width - 80 - 347
end event

type pb_excel from w_para_informes`pb_excel within w_mant_cajasprod_cambia_caracteristicas
integer x = 4174
integer y = 1436
end type

type st_computador from w_para_informes`st_computador within w_mant_cajasprod_cambia_caracteristicas
end type

type st_usuario from w_para_informes`st_usuario within w_mant_cajasprod_cambia_caracteristicas
end type

type st_temporada from w_para_informes`st_temporada within w_mant_cajasprod_cambia_caracteristicas
end type

type p_logo from w_para_informes`p_logo within w_mant_cajasprod_cambia_caracteristicas
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_mant_cajasprod_cambia_caracteristicas
integer width = 3822
string text = "Cambio de Caracteristicas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_mant_cajasprod_cambia_caracteristicas
integer x = 4169
integer y = 708
integer taborder = 110
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
end type

event pb_acepta::clicked;Integer 	li_fila
Boolean	lb_flag = False

IF em_emb.Text <> '' AND Len(em_emb.Text) > 0 THEN
	FOR li_fila = 1 TO dw_lotes.RowCount()
		dw_lotes.Object.emba_codigo[li_fila]	=	em_emb.Text
	NEXT
	lb_flag = True
END IF

IF dw_cat.Object.cate_codigo[1] > 0 THEN
	FOR li_fila = 1 TO dw_lotes.RowCount()
		dw_lotes.Object.cate_codigo[li_fila]	=	dw_cat.Object.cate_codigo[1]
	NEXT
	lb_flag = True
END IF

IF dw_eti.Object.etiq_codigo[1] > 0 THEN
	FOR li_fila = 1 TO dw_lotes.RowCount()
		dw_lotes.Object.etiq_codigo[li_fila]	=	dw_eti.Object.etiq_codigo[1]
	NEXT
	lb_flag = True
END IF

IF em_cal.Text <> '' AND Len(em_cal.Text) > 0 THEN
	FOR li_fila = 1 TO dw_lotes.RowCount()
		dw_lotes.Object.pafr_calibr[li_fila]	=	em_cal.Text
	NEXT
	lb_flag = True
END IF

IF lb_flag THEN
	Parent.TriggerEvent("ue_guardar")
END IF

IF MessageBox("Compactos", "¿Desea imprimir set de compactos para las cajas modificadas?", Question!, YesNo!) = 1 THEN
	Parent.TriggerEVent("ue_imprimir")
END IF

Parent.TriggerEvent("ue_recuperadatos")
end event

type pb_salir from w_para_informes`pb_salir within w_mant_cajasprod_cambia_caracteristicas
integer x = 4169
integer y = 1096
integer taborder = 130
end type

type gb_4 from groupbox within w_mant_cajasprod_cambia_caracteristicas
integer x = 603
integer y = 732
integer width = 3122
integer height = 244
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Datos a cambiar"
end type

type st_3 from statictext within w_mant_cajasprod_cambia_caracteristicas
integer x = 539
integer y = 480
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -9
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

type st_4 from statictext within w_mant_cajasprod_cambia_caracteristicas
integer x = 539
integer y = 584
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "N° de Pallet"
boolean focusrectangle = false
end type

type pb_1 from picturebutton within w_mant_cajasprod_cambia_caracteristicas
integer x = 1294
integer y = 572
integer width = 101
integer height = 92
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
boolean originalsize = true
end type

event clicked;Long							ll_fila
String 						ls_Null
Str_Busqueda				lstr_Busq
uo_spro_palletencab		luo_spro_palletencab

SetNull(ls_Null)
luo_spro_palletencab	=	Create uo_spro_palletencab


lstr_Busq.Argum[1] =	String(uo_SelPlanta.Codigo)
lstr_Busq.Argum[2] = "0"
lstr_Busq.Argum[3] =	"0"
lstr_Busq.Argum[4] = "0"//istr_Mant.Argumento[8]
lstr_Busq.Argum[5] =	String(uo_SelCliente.Codigo)

OpenWithParm(w_busc_pallet_movimiento, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF UpperBound(lstr_Busq.Argum) > 2 Then
	IF lstr_Busq.Argum[2] <> "" THEN
		em_recepcion.Text	=	lstr_Busq.Argum[2]
	
		IF luo_spro_palletencab.Existe(uo_SelCliente.Codigo,  uo_SelPlanta.Codigo, &
												 Long(lstr_Busq.Argum[2]),False,SqlCa) THEN
												 
			Parent.TriggerEvent("ue_recuperadatos")
			
			For ll_fila	=	1 TO dw_lotes.RowCount()
				dw_lotes.SelectRow(ll_fila, True)
			Next
		
			IF ll_fila	>	1 THEN
				rb_1.Checked = TRUE
			END IF 
		ELSE
			MessageBox("Error de Datos","El pallet ingresado pertenece a otra especie.")
			em_recepcion.Text	=	ls_Null
			Return
		END IF
	END IF
End IF
end event

type rb_2 from radiobutton within w_mant_cajasprod_cambia_caracteristicas
boolean visible = false
integer x = 2894
integer y = 1944
integer width = 622
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Desaplicar Todos"
end type

event clicked;Long	ll_fila
FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(ll_fila, False)
NEXT 
end event

type st_5 from statictext within w_mant_cajasprod_cambia_caracteristicas
integer x = 251
integer y = 1008
integer width = 3822
integer height = 924
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_cajasprod_cambia_caracteristicas
integer x = 251
integer y = 444
integer width = 3822
integer height = 256
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_recepcion from editmask within w_mant_cajasprod_cambia_caracteristicas
integer x = 910
integer y = 576
integer width = 370
integer height = 80
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;long ll_fila
istr_busq.Argum[1]	=	' '
istr_busq.Argum[2]	=  ' '
istr_busq.Argum[3]	=  ' '
istr_busq.Argum[5]	=  ' '

istr_busq.Argum[1]	=	String(uo_SelPlanta.Codigo)
istr_busq.Argum[2]	=  '1'
istr_busq.Argum[3]	=  em_recepcion.Text
istr_busq.Argum[5]	=  '0'
istr_busq.Argum[4]	=  String(uo_SelCliente.Codigo)


IF This.Text = "" OR IsNull(This.Text) THEN RETURN 

Parent.TriggerEvent("ue_recuperadatos")

FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(0, TRUE)
NEXT 

IF ll_fila	>	1 THEN
 rb_1.Checked = True
END IF 

end event

type dw_1 from datawindow within w_mant_cajasprod_cambia_caracteristicas
boolean visible = false
integer x = 5
integer y = 396
integer width = 155
integer height = 132
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_uvas"
end type

event retrievestart;Return 2
end event

type st_6 from statictext within w_mant_cajasprod_cambia_caracteristicas
integer x = 2203
integer y = 480
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -9
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

type uo_selmercado from uo_seleccion_mercados within w_mant_cajasprod_cambia_caracteristicas
integer x = 2478
integer y = 580
integer height = 84
boolean bringtotop = true
end type

on uo_selmercado.destroy
call uo_seleccion_mercados::destroy
end on

type st_7 from statictext within w_mant_cajasprod_cambia_caracteristicas
integer x = 2203
integer y = 588
integer width = 256
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Mercado"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_mant_cajasprod_cambia_caracteristicas
event destroy ( )
integer x = 910
integer y = 472
integer height = 84
integer taborder = 50
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_mant_cajasprod_cambia_caracteristicas
event destroy ( )
integer x = 2478
integer y = 472
integer height = 84
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;IF uo_SelCliente.Codigo = 81 THEN
	sle_camara.Visible		=	False
	st_7.Text					=	"Mercado"
	uo_selmercado.Visible	=	True
ELSE
	sle_camara.Visible		=	True
	st_7.Text					=	"Camara"
	uo_selmercado.Visible	=	False
END IF
end event

type rb_1 from radiobutton within w_mant_cajasprod_cambia_caracteristicas
boolean visible = false
integer x = 2427
integer y = 1948
integer width = 622
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Aplicar Todos"
boolean checked = true
end type

event clicked;Long	ll_fila
FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(ll_fila, True)
NEXT 
end event

type st_9 from statictext within w_mant_cajasprod_cambia_caracteristicas
integer x = 640
integer y = 848
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -9
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

type em_emb from editmask within w_mant_cajasprod_cambia_caracteristicas
integer x = 983
integer y = 840
integer width = 288
integer height = 80
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "!!!!!!!!!!"
end type

event modified;If IsNull(This.Text) Then Return

IF NOT iuo_emba.Existe(uo_SelCliente.Codigo, This.Text, True, Sqlca) THEN
	This.Text	=	''
	Return
END IF
end event

type st_10 from statictext within w_mant_cajasprod_cambia_caracteristicas
integer x = 1303
integer y = 848
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Categoria"
boolean focusrectangle = false
end type

type st_11 from statictext within w_mant_cajasprod_cambia_caracteristicas
integer x = 2103
integer y = 848
integer width = 261
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Etiqueta"
boolean focusrectangle = false
end type

type st_12 from statictext within w_mant_cajasprod_cambia_caracteristicas
integer x = 3122
integer y = 848
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Calibre"
boolean focusrectangle = false
end type

type em_cal from editmask within w_mant_cajasprod_cambia_caracteristicas
integer x = 3365
integer y = 840
integer width = 306
integer height = 80
integer taborder = 90
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "!!!!!"
end type

event modified;If IsNull(This.Text) or This.Text = '' Then Return

IF Not iuo_cali.Existe(em_emb.text, dw_lotes.Object.espe_codigo[1], This.Text, uo_SelCliente.Codigo, True, SQLCa) THEN
	This.Text = ''
	This.SetFocus()
END IF
end event

type dw_cat from datawindow within w_mant_cajasprod_cambia_caracteristicas
integer x = 1605
integer y = 840
integer width = 462
integer height = 84
integer taborder = 70
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_categorias_chico"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_columna
Integer	li_null

SetNull(li_null)
ls_columna	=	dwo.name

CHOOSE CASE ls_columna
	CASE "cate_codigo"
		IF NOT iuo_cate.Existe(Integer(data), True, Sqlca) THEN
			This.Object.cate_codigo[Row]	=	li_null
			Return 1
		END IF
		
END CHOOSE
end event

event itemerror;Return 1
end event

type dw_eti from datawindow within w_mant_cajasprod_cambia_caracteristicas
integer x = 2377
integer y = 840
integer width = 727
integer height = 80
integer taborder = 80
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_etiquetas_chico"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_columna
Integer	li_null

SetNull(li_null)
ls_columna	=	dwo.name

CHOOSE CASE ls_columna
	CASE "etiq_codigo"
		IF NOT iuo_etiq.Existe(Integer(data), True, Sqlca) THEN
			This.Object.etiq_codigo[Row]	=	li_null
			Return 1
		END IF
		
END CHOOSE
end event

event itemerror;Return 1
end event

type dw_2 from datawindow within w_mant_cajasprod_cambia_caracteristicas
integer x = 283
integer y = 1032
integer width = 3749
integer height = 876
integer taborder = 100
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle Original del Pallet"
string dataobject = "dw_mues_grupo_pallet"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean border = false
boolean livescroll = true
end type

event doubleclicked;IF Row > 0 THEN
	em_emb.text							=	This.Object.emba_codigo[Row]
	dw_cat.Object.cate_codigo[1]	=	This.Object.cate_codigo[Row]
	dw_eti.Object.etiq_codigo[1]	=	This.Object.etiq_codigo[Row]
	em_cal.text							=	This.Object.pafr_calibr[Row]
	
	em_emb.Enabled						=	True
	dw_cat.Enabled						=	True
	dw_eti.Enabled						=	True
	em_cal.Enabled						=	True
	
	This.SelectRow(0, False)
	This.SelectRow(Row, True)
END IF
end event

type st_8 from statictext within w_mant_cajasprod_cambia_caracteristicas
integer x = 251
integer y = 700
integer width = 3822
integer height = 308
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_nuevo from picturebutton within w_mant_cajasprod_cambia_caracteristicas
integer x = 4169
integer y = 400
integer width = 302
integer height = 244
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Documento.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Documento-bn.png"
alignment htextalign = right!
end type

event clicked;Parent.TriggerEvent("ue_nuevo")
end event

type sle_camara from editmask within w_mant_cajasprod_cambia_caracteristicas
integer x = 1417
integer y = 580
integer width = 699
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type dw_lotes from datawindow within w_mant_cajasprod_cambia_caracteristicas
boolean visible = false
integer x = 5
integer y = 244
integer width = 155
integer height = 132
integer taborder = 30
boolean bringtotop = true
string title = "CAJAS"
string dataobject = "dw_mues_palletfruta_elimina_imprime"
end type

event clicked;IF row = 0 THEN RETURN

IF IsSelected(Row) THEN
	SelectRow (Row, FALSE)
ELSE
	SelectRow (Row, TRUE)
END IF


end event

