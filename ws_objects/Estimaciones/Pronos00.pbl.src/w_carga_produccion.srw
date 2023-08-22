$PBExportHeader$w_carga_produccion.srw
$PBExportComments$Ventana de Informe de Resumen de palnillas de evaluación de descarte de fruta de packing
forward
global type w_carga_produccion from w_para_informes
end type
type st_33 from statictext within w_carga_produccion
end type
type st_14 from statictext within w_carga_produccion
end type
type st_3 from statictext within w_carga_produccion
end type
type st_5 from statictext within w_carga_produccion
end type
type st_1 from statictext within w_carga_produccion
end type
type st_2 from statictext within w_carga_produccion
end type
type st_4 from statictext within w_carga_produccion
end type
type st_6 from statictext within w_carga_produccion
end type
type uo_selproductor from uo_seleccion_productor within w_carga_produccion
end type
type st_7 from statictext within w_carga_produccion
end type
type uo_selespecie from uo_seleccion_especie within w_carga_produccion
end type
type uo_selpredio from uo_seleccion_prodpredio within w_carga_produccion
end type
type uo_selcuartel from uo_seleccion_prodcuarteles within w_carga_produccion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_carga_produccion
end type
type uo_selvariedad from uo_seleccion_variedad within w_carga_produccion
end type
type uo_selconexion from uo_seleccion_conectividad within w_carga_produccion
end type
type st_8 from statictext within w_carga_produccion
end type
type dw_1 from uo_dw within w_carga_produccion
end type
type dw_2 from uo_dw within w_carga_produccion
end type
type st_9 from statictext within w_carga_produccion
end type
type st_mensaje from statictext within w_carga_produccion
end type
end forward

global type w_carga_produccion from w_para_informes
integer x = 14
integer y = 32
integer width = 3643
integer height = 1792
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
st_33 st_33
st_14 st_14
st_3 st_3
st_5 st_5
st_1 st_1
st_2 st_2
st_4 st_4
st_6 st_6
uo_selproductor uo_selproductor
st_7 st_7
uo_selespecie uo_selespecie
uo_selpredio uo_selpredio
uo_selcuartel uo_selcuartel
uo_selcliente uo_selcliente
uo_selvariedad uo_selvariedad
uo_selconexion uo_selconexion
st_8 st_8
dw_1 dw_1
dw_2 dw_2
st_9 st_9
st_mensaje st_mensaje
end type
global w_carga_produccion w_carga_produccion

type variables
Transaction	SqlProd
Boolean		ib_Conectado
end variables

forward prototypes
public function boolean wf_conexion (string as_dbms, string as_servidor, string as_basedatos, string as_odbc, string as_usuario, string as_password, string as_ip, ref transaction at_sqlpro)
public function boolean wf_cargadistribucion ()
public function boolean wf_guardar ()
end prototypes

public function boolean wf_conexion (string as_dbms, string as_servidor, string as_basedatos, string as_odbc, string as_usuario, string as_password, string as_ip, ref transaction at_sqlpro);SetPointer(HourGlass!)

at_SqlPro.SQLCode	=	1

If ib_Conectado Then
	DISCONNECT USING at_sqlpro;
End If	

If as_DBMS = 'ODBC' Then
	at_SqlPro.ServerName	=	as_Servidor
	at_SqlPro.DataBase		=	as_BaseDatos
	at_SqlPro.Dbms			= 	as_DBMS
	at_SqlPro.DbParm			= "ConnectString='DSN=" + as_ODBC + ";UID=" + as_Usuario + ";PWD=" + as_Password + &
									"',DisableBind=1," + "ConnectOption='SQL_DRIVER_CONNECT,SQL_DRIVER_NOPROMPT'" + &
									"// ;PBUseProcOwner = " + '"Yes"'
ElseIf Mid(as_DBMS,1,3) = 'SNC' or Mid(as_DBMS,1,9) = 'TRACE SNC' Then
	at_SqlPro.Dbms			= 	as_DBMS
	at_SqlPro.ServerName	=	as_IP
	at_SqlPro.DataBase		=	as_BaseDatos
	at_SqlPro.LogId			=	as_Usuario
	at_SqlPro.LogPass			=	as_Password
	at_SqlPro.Autocommit	= True
	at_SqlPro.DbParm			=	"Provider='SQLNCLI11',Database='"+as_BaseDatos+"',TrimSpaces=1," + &
									"DbParm=DateFormat='\''yyyy/mm/dd\''',DateTimeFormat='\''yyyy/mm/dd hh:mm:ss\''',TimeFormat='\''hh:mm:ss\'''"
End If

CONNECT USING at_sqlpro;

If at_sqlpro.SQLCode = 0 Then
	ib_Conectado	=	True
Else
	ib_Conectado	=	False
End If

SetPointer(Arrow!)

Return ib_Conectado
end function

public function boolean wf_cargadistribucion ();Boolean	lb_Retorno
Long		ll_Fila, ll_New, ll_Carga

ll_Fila	= dw_2.Retrieve(uo_SelCliente.Codigo, uo_SelProductor.Codigo, uo_SelPredio.Codigo, &
							uo_SelCuartel.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo)							
If ll_Fila > 0 Then
	For ll_Carga = 1 To dw_2.RowCount()
		ll_New	= dw_1.InsertRow(0)
			
		dw_1.Object.pate_tempor[ll_New]		= dw_2.Object.pate_tempor[ll_Carga]
		dw_1.Object.prod_codigo[ll_New]		= dw_2.Object.prod_codigo[ll_Carga]
		dw_1.Object.prpr_codigo[ll_New] 		= dw_2.Object.pafr_huert1[ll_Carga]
		dw_1.Object.prcc_codigo[ll_New] 		= dw_2.Object.pafr_cuart1[ll_Carga]
		dw_1.Object.espe_codigo[ll_New]		= dw_2.Object.espe_codigo[ll_Carga]
		dw_1.Object.vari_codigo[ll_New] 		= dw_2.Object.vari_codigo[ll_Carga]
		dw_1.Object.vaca_calibr[ll_New] 		= dw_2.Object.pafh_calibr[ll_Carga]
		dw_1.Object.calm_codigo[ll_New] 	= dw_2.Object.calm_codigo[ll_Carga]
		dw_1.Object.dipr_semana[ll_New] 	= dw_2.Object.Semana[ll_Carga]
		dw_1.Object.dipr_fecsem[ll_New] 	= dw_2.Object.LunesSemana[ll_Carga]
		dw_1.Object.dipr_procen[ll_New] 		= dw_2.Object.Porcentaje[ll_Carga]
		dw_1.Object.dipr_ccajas[ll_New] 		= dw_2.Object.Cajas_Estima[ll_Carga]
	Next
	
	lb_Retorno = True
Else
	lb_Retorno = False
End If

Return lb_Retorno
end function

public function boolean wf_guardar ();Boolean		lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 then 
	Commit;
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
		dw_1.ResetUpdate()
	END IF
ELSE
	RollBack;
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

on w_carga_produccion.create
int iCurrent
call super::create
this.st_33=create st_33
this.st_14=create st_14
this.st_3=create st_3
this.st_5=create st_5
this.st_1=create st_1
this.st_2=create st_2
this.st_4=create st_4
this.st_6=create st_6
this.uo_selproductor=create uo_selproductor
this.st_7=create st_7
this.uo_selespecie=create uo_selespecie
this.uo_selpredio=create uo_selpredio
this.uo_selcuartel=create uo_selcuartel
this.uo_selcliente=create uo_selcliente
this.uo_selvariedad=create uo_selvariedad
this.uo_selconexion=create uo_selconexion
this.st_8=create st_8
this.dw_1=create dw_1
this.dw_2=create dw_2
this.st_9=create st_9
this.st_mensaje=create st_mensaje
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_33
this.Control[iCurrent+2]=this.st_14
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.st_5
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.st_6
this.Control[iCurrent+9]=this.uo_selproductor
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.uo_selespecie
this.Control[iCurrent+12]=this.uo_selpredio
this.Control[iCurrent+13]=this.uo_selcuartel
this.Control[iCurrent+14]=this.uo_selcliente
this.Control[iCurrent+15]=this.uo_selvariedad
this.Control[iCurrent+16]=this.uo_selconexion
this.Control[iCurrent+17]=this.st_8
this.Control[iCurrent+18]=this.dw_1
this.Control[iCurrent+19]=this.dw_2
this.Control[iCurrent+20]=this.st_9
this.Control[iCurrent+21]=this.st_mensaje
end on

on w_carga_produccion.destroy
call super::destroy
destroy(this.st_33)
destroy(this.st_14)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.st_6)
destroy(this.uo_selproductor)
destroy(this.st_7)
destroy(this.uo_selespecie)
destroy(this.uo_selpredio)
destroy(this.uo_selcuartel)
destroy(this.uo_selcliente)
destroy(this.uo_selvariedad)
destroy(this.uo_selconexion)
destroy(this.st_8)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.st_9)
destroy(this.st_mensaje)
end on

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPredio.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCuartel.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelVariedad.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelConexion.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelProductor.Seleccion(True, False)
	uo_SelPredio.Seleccion(True, False)
	uo_SelCuartel.Seleccion(True, False)
	uo_SelCliente.Seleccion(False, False)
	uo_SelEspecie.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelConexion.Seleccion(False, False)
	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelCliente.Bloquear(True)
	uo_SelProductor.Filtra(-1)
	
	SqlProd	=	Create Transaction

	dw_1.SetTransObject(Sqlca)	
	st_Mensaje.Text = 'Esperando Conexion...'
End If
end event

type pb_excel from w_para_informes`pb_excel within w_carga_produccion
integer x = 3735
integer y = 556
end type

type st_computador from w_para_informes`st_computador within w_carga_produccion
end type

type st_usuario from w_para_informes`st_usuario within w_carga_produccion
end type

type st_temporada from w_para_informes`st_temporada within w_carga_produccion
end type

type p_logo from w_para_informes`p_logo within w_carga_produccion
end type

type st_titulo from w_para_informes`st_titulo within w_carga_produccion
integer x = 261
integer y = 264
integer width = 2807
integer height = 88
string text = "Carga Distribucion Produccion"
end type

type pb_acepta from w_para_informes`pb_acepta within w_carga_produccion
string tag = "Imprimir Reporte"
integer x = 3168
integer y = 568
integer taborder = 190
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Cerrar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Cerrar-bn.png"
end type

event pb_acepta::clicked;Long	ll_Fila, ll_Fila_C

ll_Fila = dw_1.Retrieve(uo_SelConexion.Temporada, uo_SelProductor.Codigo, uo_SelPredio.Codigo, &
				uo_SelCuartel.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo)

If  ll_Fila > 0 Then
	If MessageBox("Atencion...", "Ya Existe una distribucion para esta seleccion.Desea Cargar Nuevamente", Exclamation!, YesNo!, 2) = 1 Then
		If dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_1, 1, Delete!)  = -1 Then
			st_Mensaje.Text = "No se pudo Borrar Distribucion Anterior."
		Else
			st_Mensaje.Text = 'Recuperando Distribucion...'
			If Not wf_CargaDistribucion() Then 
				st_Mensaje.Text = 'No se Pudo Recuperar Distribucion...'
			Else
				If wf_Guardar() Then
					st_Mensaje.Text = 'Se Grabo Distribucion en Base de Datos...'
				Else
					st_Mensaje.Text = 'No se pudo Grabar Distribucion...'
				End If
			End If
		End If
	Else
		st_Mensaje.Text = 'Proceso Finalizado, No se cargo Distribucion...'
	End If
Else
	st_Mensaje.Text = 'Recuperando Distribucion...'
	If Not wf_CargaDistribucion() Then 
		st_Mensaje.Text = 'No se Pudo Recuperar Distribucion...'
	Else
		If wf_Guardar() Then
			st_Mensaje.Text = 'Se Grabo Distribucion en Base de Datos...'
		Else
			st_Mensaje.Text = 'No se pudo Grabar Distribucion...'
		End If
	End If
End If
end event

type pb_salir from w_para_informes`pb_salir within w_carga_produccion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3168
integer y = 848
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_33 from statictext within w_carga_produccion
integer x = 325
integer y = 836
integer width = 370
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
string text = "Predio"
boolean focusrectangle = false
end type

type st_14 from statictext within w_carga_produccion
integer x = 1783
integer y = 836
integer width = 347
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_3 from statictext within w_carga_produccion
integer x = 1783
integer y = 1112
integer width = 334
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
string text = "Variedad"
boolean focusrectangle = false
end type

type st_5 from statictext within w_carga_produccion
integer x = 261
integer y = 424
integer width = 1403
integer height = 884
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_carga_produccion
integer x = 325
integer y = 1104
integer width = 370
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
string text = "Cuartel"
boolean focusrectangle = false
end type

type st_2 from statictext within w_carga_produccion
integer x = 325
integer y = 584
integer width = 370
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_4 from statictext within w_carga_produccion
integer x = 1783
integer y = 584
integer width = 334
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_6 from statictext within w_carga_produccion
integer x = 1120
integer y = 1364
integer width = 315
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
string text = "Conexion"
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_productor within w_carga_produccion
integer x = 681
integer y = 496
integer taborder = 200
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelPredio.Todos(True)
		uo_SelPredio.Filtra(-1)
		
		uo_SelCuartel.Todos(True)
		uo_SelCuartel.Filtra(-1, -1)
		
	Case Else
		uo_SelPredio.Filtra(This.Codigo)

End Choose
end event

type st_7 from statictext within w_carga_produccion
integer x = 1664
integer y = 424
integer width = 1403
integer height = 884
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_carga_produccion
integer x = 2098
integer y = 752
integer taborder = 20
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelVariedad.Todos(True)
		uo_SelVariedad.Filtra(-1)
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)

End Choose
end event

type uo_selpredio from uo_seleccion_prodpredio within w_carga_produccion
integer x = 681
integer y = 756
integer taborder = 30
boolean bringtotop = true
end type

on uo_selpredio.destroy
call uo_seleccion_prodpredio::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelCuartel.Todos(True)
		uo_SelCuartel.Filtra(uo_SelProductor.Codigo, -1)
		
	Case Else
		uo_SelCuartel.Filtra(uo_SelProductor.Codigo, This.Codigo)

End Choose
end event

type uo_selcuartel from uo_seleccion_prodcuarteles within w_carga_produccion
integer x = 681
integer y = 1016
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcuartel.destroy
call uo_seleccion_prodcuarteles::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelEspecie.Todos(True)
		uo_SelVariedad.Todos(True)
		
	Case Else
		uo_SelEspecie.Todos(False)
		uo_SelEspecie.Inicia(This.Especie)
		uo_SelVariedad.Filtra(This.Especie)
		uo_SelVariedad.Inicia(This.Variedad)
		
End Choose
end event

type uo_selcliente from uo_seleccion_clientesprod within w_carga_produccion
integer x = 2098
integer y = 580
integer height = 88
integer taborder = 200
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_carga_produccion
integer x = 2098
integer y = 1016
integer taborder = 50
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type uo_selconexion from uo_seleccion_conectividad within w_carga_produccion
integer x = 1426
integer y = 1360
integer height = 88
integer taborder = 50
boolean bringtotop = true
end type

on uo_selconexion.destroy
call uo_seleccion_conectividad::destroy
end on

event ue_cambio;call super::ue_cambio;If Not wf_Conexion(This.DBMS, This.Servidor, This.Base, This.ODBC, This.Usuario, This.Password, This.IP, SqlProd) Then
	st_Mensaje.Text = 'No se pudo efectuar conecion a Base de Datos.'
Else
	st_Mensaje.Text = 'Conectado a Temporada (' + String(This.Temporada) + ')'
	If This.DBMS = 'ODBC' Then
		dw_2.DataObject = 'dw_carga_distribucion'
	Else
		dw_2.DataObject = 'dw_carga_distribucion_sql'
	End If
	
	dw_2.SetTransObject(SqlProd)
End If
end event

type st_8 from statictext within w_carga_produccion
integer x = 261
integer y = 1308
integer width = 2807
integer height = 192
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from uo_dw within w_carga_produccion
boolean visible = false
integer x = 3136
integer y = 48
integer width = 283
integer height = 216
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_distribproduccion"
boolean vscrollbar = false
end type

event ue_nomover;//
end event

type dw_2 from uo_dw within w_carga_produccion
boolean visible = false
integer x = 3145
integer y = 304
integer width = 283
integer height = 216
integer taborder = 40
boolean bringtotop = true
boolean vscrollbar = false
end type

type st_9 from statictext within w_carga_produccion
integer x = 261
integer y = 1500
integer width = 2807
integer height = 192
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_mensaje from statictext within w_carga_produccion
integer x = 302
integer y = 1536
integer width = 2697
integer height = 116
boolean bringtotop = true
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 65535
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

