$PBExportHeader$w_mant_mues_spro_cajasembadetalle.srw
forward
global type w_mant_mues_spro_cajasembadetalle from w_mant_directo
end type
type st_2 from statictext within w_mant_mues_spro_cajasembadetalle
end type
type st_12 from statictext within w_mant_mues_spro_cajasembadetalle
end type
type st_3 from statictext within w_mant_mues_spro_cajasembadetalle
end type
type em_fecha from editmask within w_mant_mues_spro_cajasembadetalle
end type
type gb_5 from groupbox within w_mant_mues_spro_cajasembadetalle
end type
type st_1 from statictext within w_mant_mues_spro_cajasembadetalle
end type
type sle_lectura from singlelineedit within w_mant_mues_spro_cajasembadetalle
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_spro_cajasembadetalle
end type
type st_4 from statictext within w_mant_mues_spro_cajasembadetalle
end type
type uo_sellineas from uo_seleccion_lineapacking within w_mant_mues_spro_cajasembadetalle
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_spro_cajasembadetalle
end type
end forward

global type w_mant_mues_spro_cajasembadetalle from w_mant_directo
integer width = 3282
integer height = 1908
string title = "Control Cajas"
st_2 st_2
st_12 st_12
st_3 st_3
em_fecha em_fecha
gb_5 gb_5
st_1 st_1
sle_lectura sle_lectura
uo_selcliente uo_selcliente
st_4 st_4
uo_sellineas uo_sellineas
uo_selplanta uo_selplanta
end type
global w_mant_mues_spro_cajasembadetalle w_mant_mues_spro_cajasembadetalle

type variables
Long	Folio, Embaladora, Seleccionadora, Pesadora, planta

uo_EmbalajesProd		iuo_Embalaje
uo_lineapacking		iuo_linea
uo_entidades			iuo_entidades
end variables

forward prototypes
public function boolean wf_validaregistro ()
public function boolean duplicado (long codigo)
end prototypes

public function boolean wf_validaregistro ();String	ls_Null
Integer	li_error, li_Folio, li_Seleccion, li_Pesadora, li_Embala , li_Embalaje
Long		ll_planta, ll_tarjeton
Boolean	lb_Retorno = True

SetNull(ls_Null)

IF NOT iuo_linea.Existe(uo_SelPlanta.codigo, uo_Sellineas.codigo, True, Sqlca) Then Return False

li_error			= Pos(sle_lectura.Text,"NOREAD")
li_Folio			= Pos(sle_lectura.Text,".F.")
li_Seleccion		= Pos(sle_lectura.Text,".S.")
li_Pesadora 	= Pos(sle_lectura.Text,".P.")

IF li_Pesadora = 0 AND iuo_linea.Pesadora > 0 THEN
	sle_lectura.Text	=	sle_lectura.Text + ".P." + Left('00000', 5 - Len(String(iuo_linea.Pesadora)) ) + String(iuo_linea.Pesadora)
	li_Pesadora 		= 	Pos(sle_lectura.Text,".P.")
END IF

li_Embala 		= Pos(sle_lectura.Text,".E.")
li_Embalaje 	= Pos(sle_lectura.Text,".B.")

IF li_Embalaje = 0 AND Len(iuo_linea.Embalaje) > 0 THEN
	sle_lectura.Text	=	sle_lectura.Text + ".B." + String(iuo_linea.Embalaje)
	li_Embalaje 		= 	Pos(sle_lectura.Text,".B.")
END IF

IF Len(sle_lectura.Text) < 47 THEN Return False

IF IsNull(sle_lectura.Text) OR sle_lectura.Text = "" OR sle_lectura.Text = "-1" THEN	
	sle_lectura.Text 	= 	ls_Null
	lb_Retorno 			= 	False
	sle_lectura.SetFocus()
	
ELSE	
	IF Mid(sle_lectura.Text,li_Folio ,3)  = '.F.' THEN
		IF  li_error = 0 AND li_Folio <> 0 AND li_Seleccion <> 0 AND + &
			li_Pesadora <> 0 AND li_Embala <> 0 AND li_Embalaje <> 0 THEN
			IF Long(Mid(sle_lectura.Text, li_Folio + 3 ,7)) > 0 THEN
				IF IsNumber(Mid(sle_lectura.Text,li_Folio + 3 + 4 ,8)) THEN
					ll_tarjeton	=	Long(Mid(sle_lectura.Text,li_Folio + 3 + 4 ,8))
				ELSE
					ll_tarjeton	=	Long(Mid(sle_lectura.Text,li_Folio + 3 ,7))
				END IF
				
				IF Not Duplicado(ll_tarjeton) THEN
					ll_planta			=	Long(Mid(sle_lectura.Text,li_Folio + 3, 3))//planta
					planta				=	Long(Mid(sle_lectura.Text,li_Folio + 3, 4))//planta
					
					IF ll_planta = 0 THEN
						planta = uo_selplanta.Codigo
						sle_lectura.Text	=	Replace(sle_lectura.Text, li_Folio, 3, '.F.' + String(planta, '0000'))
					END IF
					
					IF uo_selplanta.Codigo <> planta THEN
						MessageBox("Error", "El Folio leído no pertenece a la planta seleccionada", Stopsign!)
						lb_retorno	=	False
					END IF
					
					IF ll_planta = 0 THEN
						Folio				=	Long(Mid(sle_lectura.Text,li_Folio + 3 + 4 ,7))//+4 de la planta
					ELSE
						Folio				=	Long(Mid(sle_lectura.Text,li_Folio + 3 + 4 ,8))//+4 de la planta
					END IF
					
					li_Pesadora 		= 	Pos(sle_lectura.Text,".P.")
					Seleccionadora		=	Long(Mid(sle_lectura.Text,li_Seleccion + 3 , 5))
					Pesadora				=	Long(Mid(sle_lectura.Text,li_Pesadora + 3, 5))
					Embaladora			=	Long(Mid(sle_lectura.Text,li_Embala + 3, 5))
					
					IF Not iuo_Entidades.ExisteEmbaladora(Seleccionadora,True,Sqlca) Then 
						lb_Retorno 			= 	False
						Return lb_Retorno
						
					ELSEIF Not iuo_Entidades.ExisteEmbaladora(Pesadora,True,Sqlca) Then
						lb_Retorno 			= 	False
						Return lb_Retorno
						
					ELSEIF Not iuo_Entidades.ExisteEmbaladora(Embaladora,True,Sqlca) Then 
						lb_Retorno 			= 	False
						Return lb_Retorno
						
					END IF
					
					iuo_Embalaje.Existe(uo_SelCliente.Codigo, Mid(sle_lectura.Text,li_Embalaje + 3 ,4), True, Sqlca)
				ELSE//.F.6000612345678.S.11111.P.22222.E.33333.B.CE09
					sle_lectura.Text 	= 	ls_Null
					lb_Retorno = False
				END IF
			ELSE
				lb_Retorno = False
			END IF
		ELSE
			lb_Retorno = False
		END IF
	ELSE
		lb_Retorno = False
	END IF
	sle_lectura.SetFocus()
END IF

Return  lb_Retorno
end function

public function boolean duplicado (long codigo);Long		ll_fila
Boolean	lb_Retorno

ll_fila	= dw_1.Find("caem_correl	= " + String(codigo), 1, dw_1.RowCount())


IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Código de Tarjeta  ya fue ingresado anteriormente",Information!, Ok!)
	lb_Retorno = True
ELSE
	lb_Retorno = False
END IF

IF NOT lb_Retorno THEN
	
	select Count(*)
	  into :ll_fila
	  from dba.spro_cajasembadetalle
	 where caem_correl = :codigo
	   and caem_tipcor <> 'C';
	  
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_cajasembadetalle")
		lb_Retorno = True
	ELSEIF ll_fila = 0 OR IsNull(ll_fila) THEN
		lb_Retorno = False
	ELSE
		MessageBox("Error","Código de Tarjeta  ya fue ingresado anteriormente",Information!, Ok!)
		lb_Retorno = True
	END IF
	
END IF

Return lb_Retorno
end function

on w_mant_mues_spro_cajasembadetalle.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_12=create st_12
this.st_3=create st_3
this.em_fecha=create em_fecha
this.gb_5=create gb_5
this.st_1=create st_1
this.sle_lectura=create sle_lectura
this.uo_selcliente=create uo_selcliente
this.st_4=create st_4
this.uo_sellineas=create uo_sellineas
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.em_fecha
this.Control[iCurrent+5]=this.gb_5
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.sle_lectura
this.Control[iCurrent+8]=this.uo_selcliente
this.Control[iCurrent+9]=this.st_4
this.Control[iCurrent+10]=this.uo_sellineas
this.Control[iCurrent+11]=this.uo_selplanta
end on

on w_mant_mues_spro_cajasembadetalle.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_12)
destroy(this.st_3)
destroy(this.em_fecha)
destroy(this.gb_5)
destroy(this.st_1)
destroy(this.sle_lectura)
destroy(this.uo_selcliente)
destroy(this.st_4)
destroy(this.uo_sellineas)
destroy(this.uo_selplanta)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_sellineas.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCliente.Seleccion(False,False)
	uo_SelPlanta.Seleccion(False,False)
	uo_sellineas.Seleccion(False,False)
	
	iuo_Embalaje		=	Create 	uo_EmbalajesProd
	iuo_Linea				=	Create 	uo_lineapacking
	iuo_Entidades		=	Create	uo_entidades
	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gstr_paramplanta.CodigoPlanta)
	uo_SelLineas.Filtra(uo_SelPlanta.Codigo)
	uo_SelLineas.Inicia(1)
	em_fecha.Text	=	String(Date(Today()))	
	
	PostEvent('ue_recuperadatos')
	
	buscar		= "Correlativo:Ncaem_correl,Seleccion:NSeleccionadora,Pesaje:NPesadora,Embalaje:NEmbaladora"
	ordenar		= "Correlativo:caem_correl,Seleccion:Seleccionadora,Pesaje:Pesadora,Embalaje:Embaladora"
End If
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelPlanta.codigo, uo_SelCliente.Codigo, Folio, iuo_Embalaje.Codigo, &
									Embaladora, Seleccionadora, Pesadora, Date(em_fecha.Text), uo_sellineas.codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
		Rollback;
	ELSEIF ll_fila > 0 THEN
		Commit;
		dw_1.SetRow(1)
		dw_1.SetFocus()
		
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_imprimir.Enabled	= True
		
		uo_SelCliente.Bloquear(True)
		uo_SelPlanta.Bloquear(True)
		em_fecha.Enabled		=	False
		
		sle_lectura.SetFocus()
		dw_1.ScrollToRow(dw_1.RowCount())
		
		il_fila					= 1
	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

sle_lectura.BackColor = RGB(63,136,193)
sle_lectura.SetFocus()

IF respuesta = 2 THEN Close(This)
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_spro_cajasembadetalle
boolean visible = false
integer x = 123
integer y = 1880
integer width = 2597
integer height = 348
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_spro_cajasembadetalle
integer x = 2967
integer y = 420
end type

event pb_nuevo::clicked;call super::clicked;uo_SelCliente.LimpiarDatos()
uo_SelPlanta.LimpiarDatos()

uo_SelCliente.Bloquear(False)
uo_SelPlanta.Bloquear(False)

uo_SelCliente.dw_Seleccion.Object.codigo[1]	= gi_CodExport
uo_SelPlanta.dw_Seleccion.Object.codigo[1]	= gstr_paramplanta.codigoplanta

uo_SelCliente.Codigo	= gi_CodExport
uo_SelPlanta.Codigo	= gstr_paramplanta.codigoplanta

em_fecha.Enabled		=	True
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_spro_cajasembadetalle
boolean visible = false
integer x = 2958
integer y = 184
boolean enabled = false
end type

event pb_lectura::clicked;call super::clicked;Em_Fecha.Enabled		=	False
sle_lectura.Enabled		=	True
sle_lectura.SetFocus()

Parent.PostEvent("ue_recuperadatos")

end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_spro_cajasembadetalle
boolean visible = false
integer x = 2958
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_spro_cajasembadetalle
boolean visible = false
integer x = 2958
end type

event pb_insertar::clicked;IF wf_ValidaRegistro() THEN
	Parent.TriggerEvent("ue_recuperadatos")
	sle_lectura.BackColor = RGB(0,255,0)
	sle_lectura.Text		= ''
	Folio	= -1
ELSE
	If sle_lectura.Text = '' Then 
		sle_lectura.BackColor = RGB(63,136,193)
	Else
		sle_lectura.BackColor = RGB(255,0,0)
	End If
	
	sle_lectura.SetFocus()
END IF
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_spro_cajasembadetalle
integer x = 2958
integer y = 1452
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_spro_cajasembadetalle
boolean visible = false
integer x = 2958
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_spro_cajasembadetalle
boolean visible = false
integer x = 2958
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_spro_cajasembadetalle
event ue_nomover pbm_syscommand
integer x = 73
integer y = 664
integer width = 2798
integer height = 780
boolean titlebar = true
string dataobject = "dw_spro_cajasembadetalle"
end type

type st_2 from statictext within w_mant_mues_spro_cajasembadetalle
string tag = "Selección Planta Packing"
integer x = 1573
integer y = 124
integer width = 206
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
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_12 from statictext within w_mant_mues_spro_cajasembadetalle
integer x = 192
integer y = 128
integer width = 261
integer height = 68
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_spro_cajasembadetalle
string tag = "Selección Planta Packing"
integer x = 1573
integer y = 236
integer width = 206
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
string text = "Fecha"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_spro_cajasembadetalle
integer x = 1797
integer y = 224
integer width = 526
integer height = 88
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;Parent.TriggerEvent('ue_recuperadatos')
end event

type gb_5 from groupbox within w_mant_mues_spro_cajasembadetalle
integer x = 137
integer y = 376
integer width = 2633
integer height = 220
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
end type

type st_1 from statictext within w_mant_mues_spro_cajasembadetalle
integer x = 73
integer y = 40
integer width = 2798
integer height = 596
integer textsize = -10
integer weight = 400
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

type sle_lectura from singlelineedit within w_mant_mues_spro_cajasembadetalle
integer x = 160
integer y = 444
integer width = 2587
integer height = 112
integer taborder = 20
boolean bringtotop = true
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 8388608
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;pb_insertar.PostEvent(Clicked!)
end event

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_spro_cajasembadetalle
event destroy ( )
integer x = 544
integer y = 120
integer height = 80
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;Parent.TriggerEvent('ue_recuperadatos')
end event

type st_4 from statictext within w_mant_mues_spro_cajasembadetalle
integer x = 192
integer y = 236
integer width = 261
integer height = 68
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
string text = "Línea"
boolean focusrectangle = false
end type

type uo_sellineas from uo_seleccion_lineapacking within w_mant_mues_spro_cajasembadetalle
integer x = 544
integer y = 228
integer height = 80
integer taborder = 40
boolean bringtotop = true
end type

on uo_sellineas.destroy
call uo_seleccion_lineapacking::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_spro_cajasembadetalle
event destroy ( )
integer x = 1797
integer y = 120
integer height = 80
integer taborder = 50
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;Parent.TriggerEvent('ue_recuperadatos')
end event

