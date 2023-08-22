$PBExportHeader$w_mant_cambiodecajas.srw
forward
global type w_mant_cambiodecajas from w_mant_directo
end type
type st_1 from statictext within w_mant_cambiodecajas
end type
type st_2 from statictext within w_mant_cambiodecajas
end type
type st_3 from statictext within w_mant_cambiodecajas
end type
type sle_caja from singlelineedit within w_mant_cambiodecajas
end type
type uo_selclientes from uo_seleccion_clientesprod within w_mant_cambiodecajas
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_cambiodecajas
end type
end forward

global type w_mant_cambiodecajas from w_mant_directo
integer width = 2519
integer height = 1136
string title = "Mantención de Tarjas - Cambio de Proceso"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
event ue_validapassword ( )
st_1 st_1
st_2 st_2
st_3 st_3
sle_caja sle_caja
uo_selclientes uo_selclientes
uo_selplanta uo_selplanta
end type
global w_mant_cambiodecajas w_mant_cambiodecajas

type variables
Integer 				ii_nro, ii_especie, ii_tipo
Long					ii_caja, il_proceso

DataWindowChild	idc_categorias
end variables

forward prototypes
public subroutine wf_bloqueacolumnas (boolean ab_bloquea)
public subroutine buscaorden ()
protected function boolean wf_actualiza_db ()
public subroutine reimprime_compacto ()
public function boolean validaproceso ()
end prototypes

event ue_validapassword();str_mant	lstr_mant

//lstr_mant.Argumento[1]	=	"Granel - Genera Cuadratura Proceso"
//lstr_mant.Argumento[2]	=	gstr_paramplanta.PassPack
//
//OpenWithParm(w_password, lstr_mant)
//
//lstr_mant	=	Message.PowerObjectParm
//
//IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public subroutine wf_bloqueacolumnas (boolean ab_bloquea);//
end subroutine

public subroutine buscaorden ();//
end subroutine

protected function boolean wf_actualiza_db ();boolean			lb_Retorno
Integer			li_especie, li_etiqueta, li_categoria, li_error
String			ls_embalaje

dw_1.AcceptText()

IF dw_1.ModifiedCount() > 0 THEN
	DECLARE ModificaCajas PROCEDURE FOR dbo.FComer_cambia_tarja    
		@Tarja 			=	:ii_caja,
		@Especie		=	:ii_especie,
		@Proceso		=	:il_proceso,
		@Cliente 		=	:uo_SelClientes.Codigo,
		@Tipo			=	:ii_tipo using sqlca;

	Execute ModificaCajas;

	IF sqlca.SQLCode = -1 THEN
		Rollback;
		F_ErrorBaseDatos(sqlca, "Cambio de Tarja")
		lb_Retorno	=	False
		dw_1.Reset()
		dw_1.InsertRow(0)
		sle_caja.Text = ""
		sle_caja.SetFocus()
	ELSE
		Fetch ModificaCajas Into :li_error;
		
		IF li_error = 0 THEN
			
			Commit;
			
			IF Messagebox("Reimpresión", "¿Desea reimprimir la Tarja?", Question!, YesNo!) = 1 THEN
				reimprime_compacto()
			END IF
			
			dw_1.Reset()
			sle_caja.Text = ""
			sle_caja.SetFocus()
		ELSE
			Rollback;
			sqlca.sqlcode	=	li_error
			F_ErrorBaseDatos(sqlca, "Cambio de Tarja")
			lb_Retorno	=	False
			dw_1.Reset()
			dw_1.InsertRow(0)
			sle_caja.Text = ""
			sle_caja.SetFocus()
		END  IF
		
	END IF
	
	Close ModificaCajas;
	
END IF

Return TRUE

end function

public subroutine reimprime_compacto ();SetPointer(HourGlass!)

Long		fila, ll_lote
Integer  li_plantalote, li_espelote

istr_info.titulo	= "IDENTIFICACION FRUTA COMERCIAL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_identificacion_fruta_comercial_1"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(dw_1.object.plde_codigo[1], dw_1.object.tpmv_codigo[1], &
                          			dw_1.object.mfco_numero[1], dw_1.object.plde_codigo[1], &
								  dw_1.object.lofc_espcod[1], dw_1.object.lofc_lotefc[1], &
								  uo_SelClientes.Codigo,dw_1.object.lfcd_secuen[1])

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end subroutine

public function boolean validaproceso ();uo_spro_ordenproceso	luo_op

luo_op	=	Create uo_spro_ordenproceso

luo_op.Existe(uo_SelPlanta.Codigo, 4, ii_nro, TRUE, SQLCA, uo_SelClientes.Codigo)

IF luo_op.Estado = 5 THEN
	MessageBox("Protección Integridad de datos", "Imposible modificar Tarja, pues pertenece ~r~n"+&
				  "a un proceso que esta con Cierre Web. Ingrese Otra Caja", StopSign!)
	Return False
END IF

IF IsNull(ii_especie) THEN
	ii_especie	=	luo_op.Especie
ELSE
	IF ii_especie <> luo_op.Especie THEN
		MessageBox("Protección Integridad de datos", "Imposible modificar Tarja, " + &
																	"Pues los procesos poseen especies diferentes", StopSign!)
		Return False
	END IF
END IF
Return True
end function

on w_mant_cambiodecajas.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.sle_caja=create sle_caja
this.uo_selclientes=create uo_selclientes
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.sle_caja
this.Control[iCurrent+5]=this.uo_selclientes
this.Control[iCurrent+6]=this.uo_selplanta
end on

on w_mant_cambiodecajas.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.sle_caja)
destroy(this.uo_selclientes)
destroy(this.uo_selplanta)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Integer li_Planta
Long	ll_fila, respuesta


DO
	ll_fila	= dw_1.Retrieve(ii_caja)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_grabar.Enabled 	= 	TRUE
		ii_nro					=	dw_1.Object.lfcd_docrel[1]
		SetNull(ii_especie)
		ii_especie			=	dw_1.Object.lofc_espcod[1]
		ii_tipo					=	dw_1.Object.mfco_tipdoc[1]
		
		IF NOT ValidaProceso() THEN
			dw_1.Reset()
			
			pb_grabar.Enabled 	= 	FALSE
			sle_caja.Text = ""
			sle_caja.SetFocus()
		END IF
	ELSE
		MessageBox("Error", "La caja ingresada no existe")
		pb_grabar.Enabled 	= 	FALSE
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event open;Boolean	lb_Cerrar = False

If IsNull(uo_SelClientes.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	
	uo_SelClientes.Seleccion(False, False)
	uo_SelClientes.Inicia(gi_CodExport)
	
	dw_1.SetTransObject(Sqlca)
	
	pb_grabar.PictureName = '\Desarrollo 17\Imagenes\Botones\Guardar Como.png'
	pb_grabar.DisabledName = '\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png'
End If
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_cambiodecajas
integer x = 37
integer width = 1920
integer height = 356
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_cambiodecajas
boolean visible = false
integer x = 3141
integer y = 456
integer taborder = 80
boolean enabled = false
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_cambiodecajas
boolean visible = false
integer x = 2103
integer y = 80
integer taborder = 20
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_cambiodecajas
boolean visible = false
integer x = 3131
integer y = 880
integer taborder = 100
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_cambiodecajas
boolean visible = false
integer x = 3154
integer y = 620
integer taborder = 90
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_cambiodecajas
integer x = 2103
integer y = 752
integer taborder = 50
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_cambiodecajas
boolean visible = false
integer x = 2930
integer y = 1176
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_cambiodecajas
integer x = 2103
integer y = 464
integer taborder = 40
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_cambiodecajas
integer x = 37
integer y = 456
integer width = 1920
integer height = 532
boolean titlebar = true
string title = "Datos Originales Caja"
string dataobject = "dw_mantencion_tarja"
boolean vscrollbar = false
boolean livescroll = false
end type

event dw_1::buttonclicked;call super::buttonclicked;string ls_columna

ls_columna = dwo.Name

CHOOSE CASE ls_columna
		
	CASE "b_procesos"
		
		buscaorden()
		
END CHOOSE		
end event

event dw_1::itemchanged;call super::itemchanged;Integer					li_Null, li_cliente
String						ls_Columna, ls_Null
Str_Busqueda			lstr_busq
uo_spro_ordenproceso	luo_op

dw_1.accepttext()
SetNull(li_Null)
SetNull(ls_Null)

ls_Columna	=	dwo.Name
CHOOSE CASE ls_Columna
	CASE "lfcd_docrel"
	luo_op	=	Create uo_spro_ordenproceso
	
	IF luo_op.Existe(uo_SelPlanta.Codigo, 4, Long(data), TRUE, SQLCA, uo_SelClientes.Codigo) THEN
		
		IF luo_op.Estado = 5 THEN
			MessageBox("Protección Integridad de datos", "imposible modificar Tarjas, pues pertenece ~r~n"+&
						  "a un proceso que esta con Cierre Web. Ingrese Otra Caja", StopSign!)
			This.SetItem(row, ls_columna, li_null)
			Return 1
		ELSEIF NOT luo_op.BuscaLoteComercial(SQLCA, TRUE) THEN
			This.SetItem(row, ls_columna, li_null)
			Return 1
		ELSE
			IF IsNull(ii_especie) THEN
				ii_especie	=	luo_op.Especie
				il_proceso 	= 	Long(data)
			ELSE
				IF ii_especie <> luo_op.Especie THEN
					MessageBox("Protección Integridad de datos", "Imposible modificar Tarja, " + &
																				"Pues los procesos poseen especies diferentes", StopSign!)
					This.SetItem(row, ls_columna, li_null)
					Return 1
				ELSE
					il_proceso 	= 	Long(data)
				END IF
			END IF
		END IF
	ELSE
		This.SetItem(row, ls_columna, li_null)
		Return 1
	END IF
	
END CHOOSE
end event

event dw_1::itemerror;call super::itemerror;Return 1
end event

type st_1 from statictext within w_mant_cambiodecajas
integer x = 183
integer y = 112
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_cambiodecajas
integer x = 183
integer y = 216
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_cambiodecajas
integer x = 183
integer y = 312
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Tarja"
boolean focusrectangle = false
end type

type sle_caja from singlelineedit within w_mant_cambiodecajas
integer x = 635
integer y = 308
integer width = 466
integer height = 84
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event modified;ii_caja	=	long(sle_caja.Text)
end event

event losefocus;pb_lectura.TriggerEvent("clicked")
end event

type uo_selclientes from uo_seleccion_clientesprod within w_mant_cambiodecajas
integer x = 635
integer y = 104
integer height = 92
integer taborder = 20
boolean bringtotop = true
end type

on uo_selclientes.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_cambiodecajas
event destroy ( )
integer x = 635
integer y = 208
integer height = 88
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

