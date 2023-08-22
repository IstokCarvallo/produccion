$PBExportHeader$w_mant_cajasprod_trasvasije.srw
$PBExportComments$Ventana de Informe de Control Calidad de Daños y Defectos
forward
global type w_mant_cajasprod_trasvasije from w_para_informes
end type
type st_3 from statictext within w_mant_cajasprod_trasvasije
end type
type st_4 from statictext within w_mant_cajasprod_trasvasije
end type
type pb_1 from picturebutton within w_mant_cajasprod_trasvasije
end type
type rb_2 from radiobutton within w_mant_cajasprod_trasvasije
end type
type gb_3 from groupbox within w_mant_cajasprod_trasvasije
end type
type rb_1 from radiobutton within w_mant_cajasprod_trasvasije
end type
type dw_lotess from datawindow within w_mant_cajasprod_trasvasije
end type
type st_2 from statictext within w_mant_cajasprod_trasvasije
end type
type em_recepcion from editmask within w_mant_cajasprod_trasvasije
end type
type dw_1 from datawindow within w_mant_cajasprod_trasvasije
end type
type st_6 from statictext within w_mant_cajasprod_trasvasije
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_cajasprod_trasvasije
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_cajasprod_trasvasije
end type
type dw_2 from datawindow within w_mant_cajasprod_trasvasije
end type
type dw_4 from datawindow within w_mant_cajasprod_trasvasije
end type
type dw_lotes from uo_dw within w_mant_cajasprod_trasvasije
end type
type st_5 from statictext within w_mant_cajasprod_trasvasije
end type
type dw_3 from uo_dw within w_mant_cajasprod_trasvasije
end type
end forward

global type w_mant_cajasprod_trasvasije from w_para_informes
integer width = 4389
integer height = 2420
string title = "MOVIMIENTO DE CAJAS ENTRE PALLETS/PUCHOS EN EXISTENCIA"
event ue_recuperadatos ( )
event ue_imprimir ( )
event ue_guardar ( )
event ue_antesguardar ( )
st_3 st_3
st_4 st_4
pb_1 pb_1
rb_2 rb_2
gb_3 gb_3
rb_1 rb_1
dw_lotess dw_lotess
st_2 st_2
em_recepcion em_recepcion
dw_1 dw_1
st_6 st_6
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
dw_2 dw_2
dw_4 dw_4
dw_lotes dw_lotes
st_5 st_5
dw_3 dw_3
end type
global w_mant_cajasprod_trasvasije w_mant_cajasprod_trasvasije

type variables
uo_correlcompequipo		iuo_Equipo

uo_lotescorrelequipo_gr		iuo_correl
uo_cliente					iuo_clie

str_busqueda				istr_busq
str_mant						istr_mant

Integer						ii_Parametro, ii_sdprusia
Long							il_Inicio[], il_Final[],li_pallet_ori,li_pallet_des
String						is_formatopomaceas


Boolean						ib_impresora
uo_manejoimpresora		iuo_impresora
end variables

forward prototypes
public function boolean noexistecliente (integer cliente)
public function boolean noexisteplanta (integer planta, integer cliente)
public subroutine obtienesegmentos ()
public function boolean wf_actualiza_db ()
public function boolean enexistencia (integer ai_cliente, long al_planta, long al_pallet)
end prototypes

event ue_recuperadatos();Datawindowchild  ldwc_lotes
Long	ll_fila, respuesta

DO
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
		dw_lotes.Reset()
		dw_3.Reset()
		em_recepcion.Text	=	""
		em_recepcion.SetFocus()
		
	ELSEIF Not EnExistencia(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, long(em_recepcion.Text)) THEN
		Messagebox("Error","El Pallet No Se Encuentra En Existencia Para Esta Planta")
		dw_lotes.Reset()
		dw_3.Reset()
		em_recepcion.Text	=	""
		em_recepcion.SetFocus()
		
 	ELSE
		
		DO 
			ll_fila	= dw_3.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo)

			IF ll_fila = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
		
			ELSEIF ll_fila = 0 THEN
				Messagebox("Error","No Existen Pallets en Existencia Para Esta Planta")
				dw_lotes.Reset()
			
			ELSE
				pb_acepta.Enabled 	= 	TRUE
				gb_3.Enabled 			=	TRUE
				rb_1.Enabled 			=	TRUE
				rb_2.Enabled 			=	TRUE
				
			END IF									
		LOOP WHILE respuesta = 1
		IF respuesta = 2 THEN Close(This)
		
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir();Long		ll_fila, ll_selected, ll_cajas
Integer	li_find

ll_selected = 0

FOR ll_fila = 1 TO dw_3.RowCount()
	IF dw_3.IsSelected(ll_fila) THEN 
		ll_selected = ll_fila
		Exit
	END IF
NEXT

IF ll_selected = 0 THEN
	MessageBox("Error", "Debe seleccionar un pallet de destino para las cajas")
	Return
END IF

IF Long(em_recepcion.Text) = dw_3.Object.paen_numero[ll_selected] THEN
	MessageBox("Error", "El pallet de destino debe ser diferente del pallet original")
	Return
END IF

li_pallet_ori 		= long(em_recepcion.Text)
li_pallet_des		= dw_3.Object.paen_numero[ll_selected]

ll_cajas	=	0

FOR ll_fila = 1 TO dw_lotes.RowCount()
	IF dw_lotes.IsSelected(ll_fila) THEN 
		dw_lotes.Object.paen_numero[ll_fila]	=	dw_3.Object.paen_numero[ll_selected]
		
		FOR li_find = 1 TO dw_2.RowCount()
			IF dw_2.Object.capr_numero[li_find] = dw_lotes.Object.pafr_secuen THEN
				dw_2.Object.capr_numpal[li_find]	= dw_3.Object.paen_numero[ll_selected]
				Exit
			END IF
		NEXT
		ll_cajas ++
		
	END IF
NEXT

TriggerEvent("ue_guardar")

IF Message.DoubleParm <> -1 THEN
	MessageBox("Movimiento Exitoso", "Se han transferido " + String(ll_cajas) 		+ " " + &
				  "cajas del pallet " + em_recepcion.Text + " " + &
				  "al pallet/pucho " + String(dw_3.Object.paen_numero[ll_selected]), Information!)
	TriggerEvent("ue_recuperadatos")
END IF


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

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
	INTO		:ls_nombre
	FROM 	dbo.clientesprod
	WHERE	clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

public function boolean noexisteplanta (integer planta, integer cliente);Integer	codigo

	
	SELECT plde_codigo
	INTO	 :Codigo	
	FROM	dbo.Plantadesp
	WHERE	plde_codigo	=	:planta;
	
	IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantas" )
			Return True			
	ELSEIF sqlca.SQLCode = 100 THEN
			messagebox("Atención","No Existe Código Seleccionado en Tabla ")			
			Return True						
	END IF

	Return False


end function

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

public function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_lotes.Update(True, False) = 1 then 
	IF dw_2.Update(True, False) = 1 then 
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
			lb_Retorno	=	False
		ELSE
			lb_Retorno	=	True
				
			dw_lotes.ResetUpdate()

			/*actualiza numero de cajas en spro_palletencab*/
			DECLARE actualiza_cajas PROCEDURE FOR dbo.fgran_actualiza_spro_palletencab
					@pallet_ori		= :li_pallet_ori, 
					@pallet_des		= :li_pallet_des,
					@cliente			= :uo_SelCliente.Codigo,
					@planta			= :uo_SelPlanta.Codigo
			USING SQLCA;
			EXECUTE actualiza_cajas;
			
			IF SQLCA.SQLCode = -1 THEN
				F_ErrorBaseDatos(SQLCA,"Movimiento de Cajas Entre Pallets/Puchos en Existencia")
			END IF

			CLOSE actualiza_cajas;
										  
			dw_2.ResetUpdate()
		END IF
	ELSE
		RollBack;
		
		IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
		
		lb_Retorno	=	False
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean enexistencia (integer ai_cliente, long al_planta, long al_pallet);Integer	li_estado

select paen_estado
  into :li_estado
  from dbo.spro_palletencab
 where clie_codigo = :ai_cliente
   and plde_codigo = :al_planta
	and paen_numero = :al_pallet;
	
IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_palletencab" )
		Return False
ELSEIF sqlca.SQLCode = 100 OR li_estado <> 1 THEN
		Return False						
END IF

Return True

end function

on w_mant_cajasprod_trasvasije.create
int iCurrent
call super::create
this.st_3=create st_3
this.st_4=create st_4
this.pb_1=create pb_1
this.rb_2=create rb_2
this.gb_3=create gb_3
this.rb_1=create rb_1
this.dw_lotess=create dw_lotess
this.st_2=create st_2
this.em_recepcion=create em_recepcion
this.dw_1=create dw_1
this.st_6=create st_6
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
this.dw_2=create dw_2
this.dw_4=create dw_4
this.dw_lotes=create dw_lotes
this.st_5=create st_5
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.pb_1
this.Control[iCurrent+4]=this.rb_2
this.Control[iCurrent+5]=this.gb_3
this.Control[iCurrent+6]=this.rb_1
this.Control[iCurrent+7]=this.dw_lotess
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.em_recepcion
this.Control[iCurrent+10]=this.dw_1
this.Control[iCurrent+11]=this.st_6
this.Control[iCurrent+12]=this.uo_selplanta
this.Control[iCurrent+13]=this.uo_selcliente
this.Control[iCurrent+14]=this.dw_2
this.Control[iCurrent+15]=this.dw_4
this.Control[iCurrent+16]=this.dw_lotes
this.Control[iCurrent+17]=this.st_5
this.Control[iCurrent+18]=this.dw_3
end on

on w_mant_cajasprod_trasvasije.destroy
call super::destroy
destroy(this.st_3)
destroy(this.st_4)
destroy(this.pb_1)
destroy(this.rb_2)
destroy(this.gb_3)
destroy(this.rb_1)
destroy(this.dw_lotess)
destroy(this.st_2)
destroy(this.em_recepcion)
destroy(this.dw_1)
destroy(this.st_6)
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
destroy(this.dw_2)
destroy(this.dw_4)
destroy(this.dw_lotes)
destroy(this.st_5)
destroy(this.dw_3)
end on

event open;Boolean	lb_Cerrar

IF IsNull(uo_Selplanta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True


iuo_correl		=	Create uo_lotescorrelequipo_gr
iuo_clie			=	Create uo_cliente
iuo_impresora	=	Create uo_manejoimpresora

IF lb_Cerrar THEN
	Close(This)
ELSE
	iuo_Equipo	=	Create uo_correlcompequipo
	
	uo_SelPlanta.Seleccion(False,False)
	uo_SelCliente.Seleccion(False,False)
	
	uo_SelPlanta.dw_Seleccion.Object.codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
	uo_SelPlanta.Codigo										=	gstr_ParamPlanta.CodigoPlanta
	uo_SelCliente.dw_Seleccion.Object.Codigo[1]		=	gi_CodExport
	uo_SelCliente.Codigo										=	gi_CodExport
	
	dw_2.SetTransObject(SQLCa)
	dw_3.SetTransObject(SQLCa)
	IF uo_SelCliente.Codigo <> 81 THEN
		is_formatopomaceas		=	"dw_info_spro_cajasprod_pomaceas"
	END IF
		
	ii_Parametro 	=	1

	IF ii_Parametro = 1 THEN
		This .Title 				=	'MOVIMIENTO DE CAJAS ENTRE PALLETS/PUCHOS EN EXISTENCIA'
		st_titulo.Text				=	'MOVIMIENTO DE CAJAS ENTRE PALLETS/PUCHOS EN EXISTENCIA'
	END IF

	pb_acepta.Enabled 				=	False
	
End If
end event

event resize;call super::resize;st_titulo.Width 	= This.WorkSpaceWidth() - 700
st_2.Width		=	st_titulo.Width

st_5.Width		=	st_titulo.Width
st_5.Height		=	This.WorkSpaceHeight() - (st_titulo.Height + 800)

dw_lotes.Width	=	st_5.Width  - 100 - dw_3.Width
dw_lotes.Height=	st_5.Height - 60

dw_3.x			=	dw_lotes.x  + dw_lotes.Width + 30
dw_3.Height		=	st_5.Height - 60
end event

type pb_excel from w_para_informes`pb_excel within w_mant_cajasprod_trasvasije
integer x = 3781
integer y = 664
end type

type st_computador from w_para_informes`st_computador within w_mant_cajasprod_trasvasije
integer x = 2089
end type

type st_usuario from w_para_informes`st_usuario within w_mant_cajasprod_trasvasije
integer x = 2089
end type

type st_temporada from w_para_informes`st_temporada within w_mant_cajasprod_trasvasije
integer x = 2089
end type

type p_logo from w_para_informes`p_logo within w_mant_cajasprod_trasvasije
end type

type st_titulo from w_para_informes`st_titulo within w_mant_cajasprod_trasvasije
integer width = 3227
string text = "MOVIMIENTO DE CAJAS ENTRE PALLETS/PUCHOS EN EXISTENCIA"
end type

type pb_acepta from w_para_informes`pb_acepta within w_mant_cajasprod_trasvasije
integer x = 3867
integer y = 1096
integer taborder = 70
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
end type

event pb_acepta::clicked;Parent.TriggerEVent("ue_imprimir")
end event

type pb_salir from w_para_informes`pb_salir within w_mant_cajasprod_trasvasije
integer x = 3867
integer y = 1460
integer taborder = 80
end type

type st_3 from statictext within w_mant_cajasprod_trasvasije
integer x = 329
integer y = 608
integer width = 453
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_cajasprod_trasvasije
integer x = 329
integer y = 732
integer width = 453
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
string text = "N° de Pallet"
boolean focusrectangle = false
end type

type pb_1 from picturebutton within w_mant_cajasprod_trasvasije
integer x = 1189
integer y = 708
integer width = 123
integer height = 100
boolean bringtotop = true
integer textsize = -10
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

type rb_2 from radiobutton within w_mant_cajasprod_trasvasije
integer x = 2089
integer y = 688
integer width = 622
integer height = 80
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Desaplicar Todos"
end type

event clicked;Long	ll_fila
FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(ll_fila, False)
NEXT 
end event

type gb_3 from groupbox within w_mant_cajasprod_trasvasije
integer x = 1979
integer y = 600
integer width = 1445
integer height = 216
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Opciones"
borderstyle borderstyle = styleraised!
end type

type rb_1 from radiobutton within w_mant_cajasprod_trasvasije
integer x = 2752
integer y = 688
integer width = 622
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Aplicar Todos"
boolean checked = true
end type

event clicked;Long	ll_fila
FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(ll_fila, True)
NEXT 
end event

type dw_lotess from datawindow within w_mant_cajasprod_trasvasije
boolean visible = false
integer x = 3616
integer y = 528
integer width = 133
integer height = 96
boolean bringtotop = true
string title = "CAJAS"
string dataobject = "dw_mues_palletfruta_elimina_imprime"
borderstyle borderstyle = StyleBox!
end type

event clicked;IF row = 0 THEN RETURN

IF IsSelected(Row) THEN
	SelectRow (Row, FALSE)
ELSE
	SelectRow (Row, TRUE)
END IF


end event

type st_2 from statictext within w_mant_cajasprod_trasvasije
integer x = 251
integer y = 440
integer width = 3227
integer height = 420
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

type em_recepcion from editmask within w_mant_cajasprod_trasvasije
integer x = 809
integer y = 724
integer width = 370
integer height = 80
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
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

type dw_1 from datawindow within w_mant_cajasprod_trasvasije
boolean visible = false
integer x = 311
integer y = 1028
integer width = 1243
integer height = 832
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_pomaceas_70x52"
boolean livescroll = true
borderstyle borderstyle = styleraised!
end type

event retrievestart;Return 2
end event

type st_6 from statictext within w_mant_cajasprod_trasvasije
integer x = 329
integer y = 496
integer width = 453
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

type uo_selplanta from uo_seleccion_plantas within w_mant_cajasprod_trasvasije
event destroy ( )
integer x = 809
integer y = 600
integer height = 84
integer taborder = 70
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_mant_cajasprod_trasvasije
event destroy ( )
integer x = 809
integer y = 488
integer height = 84
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type dw_2 from datawindow within w_mant_cajasprod_trasvasije
boolean visible = false
integer x = 942
integer y = 1200
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_palletfruta_cajasprod_elimina_imprime"
boolean livescroll = true
borderstyle borderstyle = styleraised!
end type

type dw_4 from datawindow within w_mant_cajasprod_trasvasije
boolean visible = false
integer x = 3867
integer y = 528
integer width = 133
integer height = 96
integer taborder = 40
boolean bringtotop = true
string title = "Pallets Existencia"
string dataobject = "dw_mues_pallets_trasvasije"
borderstyle borderstyle = StyleBox!
end type

event clicked;IF row > 0 THEN
	This.SelectRow(0, False)
	This.SelectRow(Row, True)
	
END IF
end event

type dw_lotes from uo_dw within w_mant_cajasprod_trasvasije
integer x = 283
integer y = 912
integer width = 2496
integer height = 972
integer taborder = 11
boolean titlebar = true
string title = "CAJAS"
string dataobject = "dw_mues_palletfruta_elimina_imprime"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF row = 0 THEN RETURN

IF IsSelected(Row) THEN
	SelectRow (Row, FALSE)
ELSE
	SelectRow (Row, TRUE)
END IF


end event

type st_5 from statictext within w_mant_cajasprod_trasvasije
integer x = 251
integer y = 876
integer width = 3227
integer height = 1060
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

type dw_3 from uo_dw within w_mant_cajasprod_trasvasije
integer x = 2789
integer y = 912
integer width = 658
integer height = 972
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string title = "Pallets Existencia"
string dataobject = "dw_mues_pallets_trasvasije"
boolean minbox = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF row > 0 THEN
	This.SelectRow(0, False)
	This.SelectRow(Row, True)
	
END IF
end event

