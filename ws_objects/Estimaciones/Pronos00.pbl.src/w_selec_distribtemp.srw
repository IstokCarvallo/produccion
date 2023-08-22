$PBExportHeader$w_selec_distribtemp.srw
forward
global type w_selec_distribtemp from w_mant_detalle
end type
type dw_2 from uo_dw within w_selec_distribtemp
end type
end forward

global type w_selec_distribtemp from w_mant_detalle
integer width = 2510
integer height = 1912
string title = "Distribucion en base a Produccion "
dw_2 dw_2
end type
global w_selec_distribtemp w_selec_distribtemp

type variables
DataWindowChild	idwc_Cuartel
end variables

on w_selec_distribtemp.create
int iCurrent
call super::create
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
end on

on w_selec_distribtemp.destroy
call super::destroy
destroy(this.dw_2)
end on

event open;call super::open;Long	ll_Fila

ll_Fila = dw_1.Retrieve(Long(istr_Mant.Argumento[1]), Long(istr_Mant.Argumento[2]),&
							Long(istr_Mant.Argumento[3]), Long(istr_Mant.Argumento[4]))

If ll_Fila = 0 Then 
	MessageBox('Atencion...', 'No hay distribuciones para Productor, Predio Seleccionado.', StopSign!, OK!)	
	istr_mant.respuesta = 0
	CloseWithReturn(This, istr_mant)
End If
end event

event ue_recuperadatos;//
end event

event resize;Integer		li_posic_x, li_posic_y, li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

This.Height			=	dw_1.Height + dw_2.Height  + 400
This.Width			=	dw_2.width + 200

dw_1.x				=	78
dw_1.y				=	100	

li_posic_x			=	This.WorkSpaceWidth() - 400
li_posic_y			=	dw_1.y

If pb_acepta.Visible Then
	pb_acepta.x			=	li_posic_x
	pb_acepta.y			=	li_posic_y
	pb_acepta.width	=	li_Ancho
	pb_acepta.height	=	li_Alto
	li_posic_y+=li_Siguiente
End If

If pb_cancela.Visible Then
	pb_cancela.x		=	li_posic_x
	pb_cancela.y		=	li_posic_y
	pb_cancela.width	=	li_Ancho
	pb_cancela.height	=	li_Alto
	li_posic_y+=li_Siguiente
End If

pb_salir.x			=	li_posic_x
pb_salir.y			=	li_posic_y
pb_salir.width		=	li_Ancho
pb_salir.height		=	li_Alto
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_selec_distribtemp
boolean visible = false
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_selec_distribtemp
boolean visible = false
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_selec_distribtemp
boolean visible = false
end type

type pb_primero from w_mant_detalle`pb_primero within w_selec_distribtemp
boolean visible = false
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_selec_distribtemp
string tag = "Ver Distribucion"
boolean visible = false
integer x = 1829
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Buscar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Buscar-bn.png"
string powertiptext = "Ver Distribucion"
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_selec_distribtemp
string tag = "Distribuir"
integer x = 1829
integer y = 124
string picturename = "\Desarrollo 17\Imagenes\Botones\lista.png"
string disabledname = ""
string powertiptext = "Distribuir"
end type

event pb_acepta::clicked;Long		ll_Fila, ll_Productor, ll_Variedad
Integer	li_Especie, li_Predio, li_Temporada

For ll_Fila = 1 To dw_1.RowCount()
	If dw_1.IsSelected(ll_Fila) Then
		ll_Productor		=	dw_1.Object.prod_codigo[ll_Fila]
		li_Predio			=	dw_1.Object.prpr_codigo[ll_Fila]
		li_Especie		=	dw_1.Object.espe_codigo[ll_Fila]
		ll_Variedad		=	dw_1.Object.vari_codigo[ll_Fila]
		li_Temporada	=	dw_1.Object.pate_tempor[ll_Fila]
	End If	
Next

If ll_Productor = 0 Or li_Predio = 0 Or li_Especie = 0 Then
	MessageBox('Atencion', 'No ha seleccionada Ninguna distribucion, Seleccionar una y volver a procesar.', StopSign!, OK!)
	Return
Else
	Declare Traspasa Procedure For dbo.Pron_DistribucionProduccion
		@Productor			=	:ll_Productor,
		@Predio				=	:li_Predio,
		@Especie			=	:li_Especie,
		@Variedad			=	:ll_Variedad,
		@Temporada		=	:gstr_tempo.Temporada,
		@TemporadaOld	=	:li_Temporada
		Using SQLCA ;
	
	Execute Traspasa;	
	
	If sqlca.SQLCode = -1 Then F_ErrorBaseDatos(SQLCA,"Problema en Carga de Distribucion por Produccion" )
	Close Traspasa;
	Commit;
End If

istr_mant.respuesta = 1
CloseWithReturn(Parent, istr_mant)
end event

type pb_salir from w_mant_detalle`pb_salir within w_selec_distribtemp
integer x = 1824
integer y = 612
end type

type dw_1 from w_mant_detalle`dw_1 within w_selec_distribtemp
integer y = 104
integer width = 1294
integer height = 736
string dataobject = "dw_selec_distribtemp_resumen"
boolean vscrollbar = true
end type

event dw_1::clicked;call super::clicked;Long	ll_Fila

IF Row = 0 THEN Return 

This.SelectRow(0,False)
This.SetRow(Row)
This.SelectRow(Row,True)

dw_2.GetChild("prcc_codigo", idwc_Cuartel)
idwc_Cuartel.SetTransObject(SQLCA)
idwc_Cuartel.Retrieve(This.Object.prod_codigo[Row], This.Object.prpr_codigo[Row])

dw_2.SetTransObject(Sqlca)

ll_Fila = dw_2.Retrieve(This.Object.pate_tempor[Row], This.Object.prod_codigo[Row], This.Object.prpr_codigo[Row], &
					This.Object.espe_codigo[Row],This.Object.vari_codigo[Row])
end event

type dw_2 from uo_dw within w_selec_distribtemp
integer x = 78
integer y = 900
integer width = 2299
integer height = 892
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string title = "Visualizacion Distribucion"
string dataobject = "dw_selec_distribtemp"
boolean hscrollbar = true
end type

