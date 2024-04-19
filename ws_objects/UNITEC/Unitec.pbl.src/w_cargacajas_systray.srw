$PBExportHeader$w_cargacajas_systray.srw
forward
global type w_cargacajas_systray from w_systray
end type
type hpb_carga from hprogressbar within w_cargacajas_systray
end type
type dw_4 from datawindow within w_cargacajas_systray
end type
type pb_carga from picturebutton within w_cargacajas_systray
end type
type dw_3 from datawindow within w_cargacajas_systray
end type
type pb_lectura from picturebutton within w_cargacajas_systray
end type
type pb_salir from picturebutton within w_cargacajas_systray
end type
type dw_2 from datawindow within w_cargacajas_systray
end type
type dw_1 from datawindow within w_cargacajas_systray
end type
type notifyicondata from structure within w_cargacajas_systray
end type
end forward

type notifyicondata from structure
	long		cbsize
	long		hwnd
	long		uid
	long		uflags
	long		ucallbackmessage
	long		hicon
	character		sztip[64]
end type

global type w_cargacajas_systray from w_systray
string tag = ""
integer width = 2464
integer height = 2512
string title = "Traspaso Cajas UNITEC a Granel"
windowstate windowstate = normal!
string icon = "AppIcon!"
event ue_antesguardar ( )
event ue_guardar ( )
event ue_nuevo ( )
event ue_recuperadatos ( )
event ue_asignacion ( )
event ue_validapassword ( )
hpb_carga hpb_carga
dw_4 dw_4
pb_carga pb_carga
dw_3 dw_3
pb_lectura pb_lectura
pb_salir pb_salir
dw_2 dw_2
dw_1 dw_1
end type
global w_cargacajas_systray w_cargacajas_systray

type variables
uo_ClientesProd			iuo_Clientes
uo_PlantaDesp				iuo_Planta
uo_spro_ordenproceso	iuo_Orden

protected:
Long					il_fila
String					buscar, ordenar, is_ultimacol, ias_campo[]
Boolean				ib_datos_ok, ib_borrar, ib_ok, ib_traer, ib_deshace = True
Date					id_FechaAcceso
Time					it_HoraAcceso
end variables

forward prototypes
public function boolean wf_actualiza_db ()
public subroutine wf_buscaorden ()
public subroutine wf_conecta ()
public subroutine wf_activacajas ()
end prototypes

event ue_antesguardar();Long	ll_fila 

For ll_Fila = 1 To dw_2.RowCount()
	If dw_2.IsSelected(ll_Fila) Then
		dw_2.Object.rfpe_porter[ll_Fila] = 1
	End If	
Next
end event

event ue_guardar();IF dw_2.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

If Not wf_actualiza_db() Then
	Message.DoubleParm = -1
	Return
End If
end event

event ue_recuperadatos();long		ll_filas_e, ll_filas_d

SetPointer(HourGlass!)
PostEvent("ue_listo")

If IsNull(dw_1.Object.orpr_turno[1]) Or dw_1.Object.orpr_turno[1] = 0 Then
	MessageBox('Error','Falta ingresar el Turno', StopSign!, Ok!)
	Return
End If

ll_Filas_E		=	dw_2.Retrieve(iuo_Clientes.Codigo, iuo_Orden.NumeroOrden, dw_1.Object.orpr_turno[1], dw_1.Object.orpr_fecpro[1])
ll_Filas_D	=	dw_3.Retrieve(iuo_Clientes.Codigo, iuo_Planta.Codigo, dw_1.Object.orpr_fecpro[1])

If ll_Filas_E = -1 Or ll_filas_d = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura de Cajas UNITEC")
	dw_2.Reset()
	dw_2.SetRedraw(True)
	Return
End If

wf_ActivaCajas()

end event

event ue_asignacion();str_busqueda	lstr_Busq

lstr_Busq.Argum[1]	=	String(iuo_Clientes.Codigo)
lstr_Busq.Argum[2]	=	String(iuo_Orden.NumeroOrden)
lstr_Busq.Argum[3]	=	String(dw_2.Object.numeropalet[dw_2.GetRow()])

Timer(0)

gstr_us.OpcionActiva	=	'm_consulta'

OpenWithParm(w_detalle_cajas, lstr_busq)

Timer(180)
end event

event ue_validapassword();str_mant			lstr_mant	

lstr_mant.Argumento[1]	=	"CargaCajas"
lstr_mant.Argumento[2]	=	"rio.packing.2022"

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If dw_2.Update(True, False) = 1 Then 
	Commit;
	
	If sqlca.SQLCode <> 0 Then
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	Else
		lb_Retorno	=	True		
		dw_2.ResetUpdate()
	End If
Else
	RollBack;
	If sqlca.SQLCode <> 0 Then F_ErrorBaseDatos(sqlca, This.Title)
	lb_Retorno	=	False
End If

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
	

end function

public subroutine wf_buscaorden ();Str_Busqueda	lstr_busq
String ls_Nula

SetNull(ls_nula)

lstr_busq.argum[1]	=	String(iuo_Planta.Codigo)
lstr_busq.argum[2]	=	"4"
lstr_busq.argum[3]	=  ""
lstr_busq.argum[4]	=  String(iuo_Clientes.Codigo)

OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1] <> "0" THEN
	dw_1.SetItem(1,"orpr_numero",long(lstr_busq.argum[6]))	
ELSE
	dw_1.SetItem(1,"orpr_numero",long(ls_Nula))
END IF
end subroutine

public subroutine wf_conecta ();String		ls_server1 = "12345678", ls_nombre, ls_Dbms, ls_DbParm, ls_Provider

IF ib_connected_UNITEC THEN
	DISCONNECT Using gt_UNITEC ;
END IF

ib_connected_UNITEC	=	False

gt_UNITEC.SQLCode	=	1
ii_pos						=	1
ls_server1				=	ProfileString(gstr_apl.ini, is_base, "UNITEC_ServerName", "12345678")
ls_Dbms					=	ProFileString(gstr_apl.ini, is_base, "dbms", "ODBC")
ls_Provider				=	ProFileString(gstr_apl.ini, is_base, "Provider", "SQLNCLI10")

IF ls_server1 <> "12345678" THEN
	ls_nombre			=	ProfileString(gstr_apl.ini, is_base, "NombreOdbc", "")
	gt_UNITEC.Dbms			=	ProFileString(gstr_apl.ini, is_base, "dbms", "ODBC")
	gt_UNITEC.ServerName	=	ProfileString(gstr_apl.ini, is_base, "UNITEC_ServerName", "")
	gt_UNITEC.DataBase		=	ProFileString(gstr_apl.ini, is_base, "UNITEC_Database", "")
	gt_UNITEC.LogId			=	ProFileString(gstr_apl.ini, is_base, "UNITEC_LogId", "dba")
	gt_UNITEC.LogPass		=	ProFileString(gstr_apl.ini, is_base, "UNITEC_LogPassWord", "info")
	
	IF ls_Dbms = "ODBC" THEN
		gt_UNITEC.DbParm	=	"ConnectString='DSN=" + ls_nombre + &
								";UID=" + gt_UNITEC.LogId + &
								";PWD=" + gt_UNITEC.LogPass + "'," + &
								"ConnectOption='SQL_DRIVER_CONNECT,SQL_DRIVER_NOPROMPT'" + &
								"// ;PBUseProcOwner = " + '"Yes"'
	ElseIf Mid(ls_Dbms,1,3) = 'SNC' or Mid(ls_Dbms,1,9) = 'TRACE SNC' Then
		gt_UNITEC.Autocommit = True
			
		If Len(Trim(ls_DBParm)) > 0 Then ls_DbParm = ","+ls_DbParm
		
		ls_Dbparm = "Provider='"+ ls_Provider +"',Database='"+ProfileString(gstr_apl.ini, is_base, "UNITEC_Database", "")+"'"+ls_DbParm+",TrimSpaces=1"
			
		gt_UNITEC.DBParm = ls_Dbparm
	END IF
ELSE
	MessageBox(This.Title,"No se puede ejecutar la aplicación por la falta de archivo " &
					+ gstr_apl.ini + ". Verifique que el archivo exista y que esté en el directorio " + &
					"donde la aplicación esté corriendo o en el PATH del Computador del cliente.",StopSign!)
	Halt Close
	Return
END IF

SetPointer(HourGlass!)

CONNECT Using gt_UNITEC ; 

IF gt_UNITEC.SQLCode = 0 THEN
	ib_connected_UNITEC	= True
ELSE
	ib_connected_UNITEC	= False
END IF



end subroutine

public subroutine wf_activacajas ();Long	ll_Fila, ll_Busca
String	ls_Busca

dw_2.SetRedraw(False)

For ll_Fila = 1 To dw_2.RowCount()
	ls_Busca = 'paen_numero = ' + String(Long(dw_2.GetItemString(ll_Fila, 'numeropalet')))
	
	ll_Busca = dw_3.Find(ls_Busca, 1, dw_3.RowCount())
	
	If ll_Busca >= 1 Then 
		dw_2.Object.Borra[ll_Fila] = 1
	End If	
	
Next 

ll_Fila = 1
Do While ll_Fila <=  dw_2.RowCount()
	If dw_2.Object.Borra[ll_Fila] = 1 Then 
		dw_2.DeleteRow(ll_Fila)
	Else
		ll_Fila++
	End If
Loop

dw_2.SetRedraw(True)
end subroutine

on w_cargacajas_systray.create
int iCurrent
call super::create
this.hpb_carga=create hpb_carga
this.dw_4=create dw_4
this.pb_carga=create pb_carga
this.dw_3=create dw_3
this.pb_lectura=create pb_lectura
this.pb_salir=create pb_salir
this.dw_2=create dw_2
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.hpb_carga
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.pb_carga
this.Control[iCurrent+4]=this.dw_3
this.Control[iCurrent+5]=this.pb_lectura
this.Control[iCurrent+6]=this.pb_salir
this.Control[iCurrent+7]=this.dw_2
this.Control[iCurrent+8]=this.dw_1
end on

on w_cargacajas_systray.destroy
call super::destroy
destroy(this.hpb_carga)
destroy(this.dw_4)
destroy(this.pb_carga)
destroy(this.dw_3)
destroy(this.pb_lectura)
destroy(this.pb_salir)
destroy(this.dw_2)
destroy(this.dw_1)
end on

event open;call super::open;Int Row

x					= 	0
y					= 	0
This.Width		= 	dw_1.width + 770
This.Height		= 	2750	

iuo_Clientes	=	Create uo_ClientesProd
iuo_Planta	=	Create uo_PlantaDesp
iuo_Orden	=	Create uo_spro_ordenproceso

gt_UNITEC	=	Create Transaction

wf_conecta()
Conexion()

dw_1.SetTransObject(SQLCA)
dw_2.SetTransObject(gt_UNITEC)
dw_3.SetTransObject(SQLCA)
dw_4.SetTransObject(gt_UNITEC)

Row = dw_1.InsertRow(0)

dw_1.Object.clie_codigo[Row] = 81
dw_1.Object.plde_codigo[Row] = 41
dw_1.Object.orpr_tipord[Row] = 4
dw_1.Object.orpr_turno[Row] = 1
dw_1.Object.orpr_fecpro[Row] = Today()
iuo_Clientes.Codigo = 81
iuo_Planta.Codigo = 41
iuo_Orden.NumeroOrden = -1

PostEvent("ue_validapassword")

Timer(180)

end event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0, li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

If dw_1.width > This.Width Then
	maximo		=	dw_1.width
Else
	dw_2.width	=	This.WorkSpaceWidth() - 100
	maximo		=	dw_2.width
End If

dw_1.x					=	37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					=	37

dw_2.x					=	37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					=	84 + dw_1.Height
dw_2.height				=	This.WorkSpaceHeight() - dw_2.y - 100

hpb_carga.y				=	100 + dw_1.Height + dw_2.Height
hpb_carga.x				=	dw_2.x
hpb_carga.Width		=	dw_2.Width

li_posic_x				=	This.WorkSpaceWidth() - 370
li_posic_y				=	dw_1.y

If pb_lectura.Visible Then
	pb_lectura.x			=	li_posic_x
	pb_lectura.y			=	li_posic_y
	pb_lectura.width	=	li_Ancho
	pb_lectura.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_carga.Visible Then
	pb_carga.x			=	li_posic_x
	pb_carga.y			=	li_posic_y
	pb_carga.width		=	li_Ancho
	pb_carga.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_salir.Visible Then
	pb_salir.x			=	li_posic_x
	pb_salir.y			=	li_posic_y
	pb_salir.width		=	li_Ancho
	pb_salir.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If
end event

event ue_trayrightclicked;call super::ue_trayrightclicked;m_carga_opcion lm_menu_opcion

This.SetFocus()
lm_menu_opcion = CREATE m_carga_opcion
lm_menu_opcion.m_pop.PopMenu(PointerX(),PointerY())
DESTROY lm_menu_opcion
end event

event timer;call super::timer;Long	ll_Filas

ll_Filas	=	dw_2.Retrieve(iuo_Clientes.Codigo, iuo_Orden.NumeroOrden, dw_1.Object.orpr_turno[1], dw_1.Object.orpr_fecpro[1])

If ll_Filas = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Movimiento Casino")
	dw_2.Reset()
	dw_2.SetRedraw(True)
	Return
End If
end event

event ue_traydoubleclicked;call super::ue_traydoubleclicked;PostEvent("ue_validapassword")
end event

type hpb_carga from hprogressbar within w_cargacajas_systray
integer x = 41
integer y = 2216
integer width = 1787
integer height = 68
unsignedinteger maxposition = 100
integer setstep = 1
end type

type dw_4 from datawindow within w_cargacajas_systray
boolean visible = false
integer x = 1998
integer y = 1068
integer width = 160
integer height = 132
integer taborder = 40
string title = "Detalle de Recepciones"
string dataobject = "dw_mues_cargacajas"
borderstyle borderstyle = stylelowered!
end type

event clicked;If Row > 0 Then
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
	
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRow(il_fila)
End If

Return 0
end event

event rbuttondown;m_consulta_sis	l_Menu 

If Row =	0	Then
	Return
Else			
	gstr_us.OpcionActiva	=	Parent.ClassName()
	This.SetRow(Row)
			
	l_Menu = CREATE m_consulta_sis	
	l_Menu.m_m_edicion.m_conulta.Visible 	= TRUE
	
	l_Menu.m_m_edicion.PopMenu(Parent.PointerX(), Parent.PointerY())	
End If
end event

type pb_carga from picturebutton within w_cargacajas_systray
string tag = "Muestra Ultimo Ingreso"
integer x = 2034
integer y = 332
integer width = 302
integer height = 244
integer taborder = 30
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean flatstyle = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
string powertiptext = "Muestra Ultimo Ingreso"
long backcolor = 553648127
end type

event clicked;Long			ll_Fila, ll_Fila_D
Decimal{2}	ll_Step
String 		codCaja,  Proceso, Lote, codLinea, codTurno, Salida, codEspecie, codVariedadReal, codVariedadRotulada, codEmbalaje,&
				codConfeccion, codMarca , CalibreColor, codCAT, codProductorReal, CSGProductorReal, codClienteProductorReal, &
				codProductorRotulado, CSGProductorRotulado, codClienteProductorRotulado, codProductorCompacto_ORD, SDP_ORD, &
				codPredio_ORD, codCuartel_ORD, codCliente_ORD, NumeroPalet, codTipoPalet, codGGN
Datetime		FechaProduccion, FechaPaletizaje 
Integer		codTipoTarja

SetPointer(HourGlass!)

Timer(0)

hpb_Carga.SetRange(0,100)
hpb_Carga.StepIt()

dw_2.GroupCalc()

ll_Step = 100 / dw_2.Object.total[1]
	
For ll_Fila = 1 To dw_2.RowCount()
	hpb_Carga.Setstep = ll_Step
	If dw_2.GetItemNumber(ll_Fila, 'marca') = 1 Then
		ll_Fila_D = dw_4.Retrieve(dw_1.Object.clie_codigo[1], -1, Long(dw_2.Object.NumeroPalet[ll_Fila]))
		
		If ll_Fila_D <> -1 Then
			For ll_Fila_D = 1 To dw_4.RowCount() 
				
				codCaja 								=	dw_4.Object.codCaja [ll_Fila_D]
				FechaProduccion 					=	dw_4.Object.FechaProduccion[ll_Fila_D]
				Proceso 								=	dw_4.Object.Proceso[ll_Fila_D]
				Lote 									=	dw_4.Object.Lote[ll_Fila_D]
				codLinea 							=	dw_4.Object.codLinea[ll_Fila_D]
				codTurno 							=	dw_4.Object.codTurno[ll_Fila_D]
				Salida 								=	dw_4.Object.Salida[ll_Fila_D]
				codEspecie 							=	dw_4.Object.codEspecie[ll_Fila_D]
				codVariedadReal 					=	dw_4.Object.codVariedadReal[ll_Fila_D]
				codVariedadRotulada			 	=	dw_4.Object.codVariedadRotulada[ll_Fila_D]
				codEmbalaje 						=	dw_4.Object.codEmbalaje[ll_Fila_D]
				codConfeccion 						=	dw_4.Object.codConfeccion[ll_Fila_D]
				codMarca 							=	dw_4.Object.codMarca[ll_Fila_D]
				CalibreColor 						=	dw_4.Object.CalibreColor[ll_Fila_D]
				codCAT 								=	dw_4.Object.codCAT[ll_Fila_D]
				codProductorReal 					=	dw_4.Object.codProductorReal[ll_Fila_D]
				CSGProductorReal 				=	dw_4.Object.CSGProductorReal[ll_Fila_D]
				codClienteProductorReal 			=	dw_4.Object.codClienteProductorReal[ll_Fila_D]
				codProductorRotulado 			=	dw_4.Object.codProductorRotulado[ll_Fila_D]
				CSGProductorRotulado 			=	dw_4.Object.CSGProductorRotulado[ll_Fila_D]
				codClienteProductorRotulado 	=	dw_4.Object.codClienteProductorRotulado[ll_Fila_D]
				codProductorCompacto_ORD 	=	dw_4.Object.codProductorCompacto_ORD[ll_Fila_D]
				SDP_ORD 							=	dw_4.Object.SDP_ORD[ll_Fila_D]
				codPredio_ORD 					=	dw_4.Object.codPredio_ORD[ll_Fila_D]
				codCuartel_ORD 					=	dw_4.Object.codCuartel_ORD[ll_Fila_D]
				codCliente_ORD 					=	dw_4.Object.codCliente_ORD[ll_Fila_D]
				NumeroPalet 						=	dw_4.Object.NumeroPalet[ll_Fila_D]
				FechaPaletizaje 					=	dw_4.Object.FechaPaletizaje[ll_Fila_D]
				codTipoTarja  						=	dw_4.Object.TipoPallet[ll_Fila_D]
				codTipoPalet						=	dw_4.Object.codTipoPalet[ll_Fila_D]
				codGGN								=	f_AsignaGGN(Long(dw_4.Object.codProductorReal[ll_Fila_D]), &
																				Integer(dw_4.Object.codPredio_ORD[ll_Fila_D]), &
																				Integer(dw_4.Object.codEspecie[ll_Fila_D]),&
																				Date(dw_4.Object.FechaProduccion[ll_Fila_D]), False)

				DECLARE	InsertaCajas PROCEDURE FOR dbo.UNITEC_InsertaCajas
							@codCaja  								=	:codCaja,
							@FechaProduccion 					=	:FechaProduccion,
							@Proceso 		 						=	:Proceso,
							@Lote  									=	:Lote,
							@codLinea  								=	:codLinea,
							@codTurno  							=	:codTurno,
							@Salida  								=	:Salida,
							@codEspecie  							=	:codEspecie,
							@codVariedadReal  					=	:codVariedadReal,
							@codVariedadRotulada 			 	=	:codVariedadRotulada,
							@codEmbalaje  						=	:codEmbalaje,
							@codConfeccion  						=	:codConfeccion,
							@codMarca  							=	:codMarca,
							@CalibreColor  						=	:CalibreColor,
							@codCAT  								=	:codCAT,
							@codProductorReal  					=	:codProductorReal,
							@CSGProductorReal  					=	:CSGProductorReal,
							@codClienteProductorReal  			=	:codClienteProductorReal,
							@codProductorRotulado  			=	:codProductorRotulado,
							@CSGProductorRotulado  			=	:codClienteProductorRotulado,
							@codClienteProductorRotulado  	=	:codClienteProductorRotulado,
							@codProductorCompacto_ORD  	=	:codProductorCompacto_ORD,
							@SDP_ORD  							=	:SDP_ORD,
							@codPredio_ORD  					=	:codPredio_ORD,
							@codCuartel_ORD  					=	:codCuartel_ORD,
							@codCliente_ORD  					=	:codCliente_ORD,
							@NumeroPalet  						=	:NumeroPalet,
							@FechaPaletizaje  					=	:FechaPaletizaje,
							@codTipoTarja  						=	:codTipoTarja,
							@codTipoPalet							=	:codTipoPalet,
							@GGN									=	:codGGN
					USING SQLCA ;
							
					EXECUTE InsertaCajas  ;	
				
					If SQLCA.SQLCode = -1 Then
						F_ErrorBaseDatos(sqlca, "Lectura del Procedimiento Almacenado Inserta Cajas UNITEC" )				
						Rollback;
					End If
			Next
		Else
			MessageBox('Atencion', 'No se pudo cargar detalle de Folio:' + dw_2.Object.NumeroPalet[ll_Fila] + ', para carga de cajas', Information!, OK!)
			Return
		End If
	End If
	hpb_Carga.Stepit()
Next

Parent.PostEvent('ue_recuperadatos')

SetPointer(Arrow!)

Timer(180)
end event

type dw_3 from datawindow within w_cargacajas_systray
boolean visible = false
integer x = 1998
integer y = 900
integer width = 155
integer height = 184
integer taborder = 30
string title = "Detalle de Recepciones"
string dataobject = "dw_mues_spro_palletencab"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

event clicked;If Row > 0 Then
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
	
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRow(il_fila)
End If

Return 0
end event

event rbuttondown;m_consulta_sis	l_Menu 

If Row =	0	Then
	Return
Else			
	gstr_us.OpcionActiva	=	Parent.ClassName()
	This.SetRow(Row)
			
	l_Menu = CREATE m_consulta_sis	
	l_Menu.m_m_edicion.m_conulta.Visible 	= TRUE
	
	l_Menu.m_m_edicion.PopMenu(Parent.PointerX(), Parent.PointerY())	
End If
end event

type pb_lectura from picturebutton within w_cargacajas_systray
string tag = "Muestra Ultimo Ingreso"
integer x = 2034
integer y = 72
integer width = 302
integer height = 244
integer taborder = 10
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean flatstyle = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Busqueda.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Busqueda-bn.png"
string powertiptext = "Muestra Ultimo Ingreso"
long backcolor = 553648127
end type

event clicked;Parent.TriggerEvent("ue_recuperadatos")
end event

type pb_salir from picturebutton within w_cargacajas_systray
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2002
integer y = 548
integer width = 302
integer height = 244
integer taborder = 10
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
boolean flatstyle = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Cancelar-bn.png"
long backcolor = 553648127
end type

event clicked;Parent.Hide()
end event

type dw_2 from datawindow within w_cargacajas_systray
integer x = 160
integer y = 704
integer width = 1787
integer height = 1392
integer taborder = 20
boolean titlebar = true
string title = "Detalle de Recepciones"
string dataobject = "dw_mues_cajas"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;If Row > 0 Then
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
	
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRow(il_fila)
End If

Return 0
end event

event rbuttondown;m_consulta_sis	l_Menu 

If Row =	0	Then
	Return
Else			
	gstr_us.OpcionActiva	=	Parent.ClassName()
	This.SetRow(Row)
			
	l_Menu = CREATE m_consulta_sis	
	l_Menu.m_m_edicion.m_conulta.Visible 	= TRUE
	
	l_Menu.m_m_edicion.PopMenu(Parent.PointerX(), Parent.PointerY())	
End If
end event

type dw_1 from datawindow within w_cargacajas_systray
integer x = 41
integer y = 28
integer width = 1778
integer height = 716
integer taborder = 20
string dataobject = "dw_mant_orden"
boolean border = false
borderstyle borderstyle = styleshadowbox!
end type

event itemchanged;String		ls_columna, ls_Null

Timer(0)

SetNull(ls_null)

Timer(0)

ls_columna = dwo.Name

Choose Case ls_columna		
	Case "clie_codigo"
		If Not iuo_Clientes.Existe(Integer(Data), True, SQLCA) Then
			This.SetItem(row, ls_Columna, Integer(ls_Null))
			Return 1
		Else
			This.SetItem(Row, "orpr_numero", Integer(ls_Null))
		End If
			
	Case 'plde_codigo'
		If Not iuo_Planta.Existe(Integer(Data), True, SQLCA) Then
			This.SetItem(row, ls_Columna, Integer(ls_Null))
			Return 1
		End If
		
	Case "orpr_numero"	
		If Not iuo_Orden.Existe(iuo_Planta.Codigo, This.Object.orpr_tipord[Row], Long(data),True,Sqlca, iuo_Clientes.Codigo) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		End If
		
	Case 'todos'
		If Data = '1' Then iuo_Orden.NumeroOrden = -1
		This.SetItem(Row, "orpr_numero", Integer(ls_Null))
End Choose

Timer(180)
end event

event itemerror;Return 1	
end event

event buttonclicking;String ls_columna

Timer(0)

ls_Columna = dwo.Name

Choose Case ls_columna
	Case "b_orden"
		wf_buscaorden()
		
End Choose

Timer(180)
end event

