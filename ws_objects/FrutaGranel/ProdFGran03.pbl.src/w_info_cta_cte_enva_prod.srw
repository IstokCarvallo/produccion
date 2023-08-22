$PBExportHeader$w_info_cta_cte_enva_prod.srw
$PBExportComments$Informe de Movimiento de Envases Diarios por Plantas
forward
global type w_info_cta_cte_enva_prod from w_para_informes
end type
type dw_1 from datawindow within w_info_cta_cte_enva_prod
end type
end forward

global type w_info_cta_cte_enva_prod from w_para_informes
integer width = 2816
integer height = 1840
string title = "Cuenta Corriente Por Envases"
dw_1 dw_1
end type
global w_info_cta_cte_enva_prod w_info_cta_cte_enva_prod

type variables
DataWindowChild	idwc_planta, idwc_productor, idwc_tipoenvase, idwc_envase, &
						idwc_plantadest, idwc_calidad, idwc_Cliente

uo_plantadesp			iuo_plantadesp
uo_productores			iuo_productores
uo_ClientesProd		iuo_Cliente
uo_Envases				iuo_Envases
uo_TipoEnvases		iuo_Tipo

Integer	ii_planta,ii_Plantadest, ii_tipo_envase 
end variables

on w_info_cta_cte_enva_prod.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_cta_cte_enva_prod.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;call super::open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_productores		=	Create uo_productores
iuo_Cliente				=	Create uo_Clientesprod
iuo_Envases				=	Create uo_Envases	
iuo_Tipo					=	Create uo_TipoEnvases

////Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
If idwc_planta.Retrieve(gi_codexport) = 0 Then idwc_planta.InsertRow(0)

dw_1.GetChild("cliente", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
If idwc_cliente.Retrieve() = 0 Then  idwc_cliente.InsertRow(0)

////Planta Destino
dw_1.GetChild("plantadest", idwc_plantadest)
idwc_plantadest.SetTransObject(sqlca)
If idwc_plantadest.Retrieve(gi_codexport) = 0 Then idwc_plantadest.InsertRow(0)

//Productor
dw_1.GetChild("productor",idwc_productor)
idwc_productor.SetTransObject(Sqlca)
If idwc_productor.Retrieve(-1) = 0 Then idwc_productor.InsertRow(0)

//Tipos de Envase
dw_1.GetChild("tiposenvase",idwc_tipoenvase)
idwc_tipoenvase.SetTransObject(Sqlca)
If idwc_tipoenvase.Retrieve() = 0 Then 	idwc_tipoenvase.InsertRow(0)

//Envases
dw_1.GetChild("envase",idwc_envase)
idwc_envase.SetTransObject(Sqlca)
If idwc_envase.Retrieve(-1) = 0 Then idwc_envase.InsertRow(0)

//Calidad
dw_1.GetChild("Calidad",idwc_calidad)
idwc_calidad.SetTransObject(Sqlca)
If idwc_calidad.Retrieve(0,0) = 0 Then idwc_calidad.InsertRow(0)

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

Paramtemporada(gstr_paramtempo)
dw_1.SetItem(1,"fechad", gstr_paramtempo.fechainicio)
dw_1.SetItem(1,"fechah", Today())
end event

event resize;call super::resize;st_titulo.x					=	288
st_titulo.y					=	284
end event

type pb_excel from w_para_informes`pb_excel within w_info_cta_cte_enva_prod
end type

type st_computador from w_para_informes`st_computador within w_info_cta_cte_enva_prod
integer x = 1879
end type

type st_usuario from w_para_informes`st_usuario within w_info_cta_cte_enva_prod
integer x = 1879
end type

type st_temporada from w_para_informes`st_temporada within w_info_cta_cte_enva_prod
integer x = 1879
end type

type p_logo from w_para_informes`p_logo within w_info_cta_cte_enva_prod
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_cta_cte_enva_prod
integer x = 288
integer width = 2025
string text = "Informe Cuenta Corriente Por Envases"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cta_cte_enva_prod
integer x = 2432
integer y = 616
integer taborder = 130
end type

event pb_acepta::clicked;Integer	 li_tipoenvase, li_envase, li_Consfechad, li_grupo, &
			li_tipinf, li_TipoSaldo, li_SaldoCero, li_Orden, li_consguias, li_bodega, li_Cliente
Long		ll_Fila, ll_productor, ll_plantadest, ll_Planta
Datetime	ld_fechaHasta , ld_fechadesde, ld_fechaprueba
String 	ls_fecha, ls_Calidad

SetPointer(Arrow!)

istr_info.titulo	= 'CUENTA CORRIENTE DE ENVASES'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_cta_cte_enva_prod"
dw_1.AcceptText()

// Acepta Cliente//
If dw_1.Object.todocliente[1] = 1 Then
	li_Cliente = -1
	If dw_1.Object.conscliente[1] = 1 Then
		li_Cliente = -9
	End If
Else
	li_Cliente = dw_1.Object.cliente[1]
	If IsNull(ll_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Planta //
If dw_1.Object.todosplanta[1] = 1 Then
	ll_Planta = -1
	If dw_1.Object.consplanta[1] = 1 Then
		ll_planta = -9
	End If
Else
	ll_Planta = dw_1.Object.planta[1]
	If IsNull(ll_Planta) Then
		MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", StopSign!, Ok!)
		Return 1				 
   End If
End If

If dw_1.Object.otrasplanta[1] = 1 Then
	li_tipinf = 2
Else
	li_tipinf = 1
End If	

li_TipoSaldo		=	dw_1.Object.TipoSaldo[1]
li_SaldoCero	=	dw_1.Object.SaldoCero[1]

If li_tipinf = 2 Then
	ll_productor  = 0
	If dw_1.Object.todosprod[1] = 1 Then
   	ll_plantadest = -1
	    If dw_1.Object.consprod[1] = 1 Then	
			ll_Plantadest =	-9 //Para Consolidar Otras Plantas,de Acuerdo Tipo inf//
		End If	
	Else
		ll_Plantadest = dw_1.Object.plantadest[1]
		If IsNull(ll_plantadest) Then
			MessageBox( "Planta Destino Erróneo", "Falta seleccionar una Planta Destino.", StopSign!, Ok!)
			Return 1				 
		End If
	End If
Else	
	ll_plantadest = -1
	If dw_1.Object.todosprod[1] = 1 Then
   	ll_Productor = -1
		If dw_1.Object.consprod[1] = 1 Then	
			ll_Productor =	-9
		End If
	Else
		ll_Productor = dw_1.Object.productor[1]
		If IsNull(ll_Productor) Then
			MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.",  StopSign!, Ok!)
			Return 1				 
		End If
	End If
	If dw_1.Object.grupprod[1] = 1 Then
		li_grupo = 1
	End If
End If

// Acepta Tipos de Envase //
If dw_1.Object.todostiposenva[1] = 1 Then
	 li_tipoenvase = -1
Else
	 li_tipoenvase = dw_1.Object.tiposenvase[1]
	If IsNull( li_tipoenvase) Then
		MessageBox( "Tipo de Envase Erróneo", "Falta seleccionar un Tipo de Envase.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Envase //
If dw_1.Object.todosenva[1] = 1 Then
	li_envase = -1
Else
	 li_envase = dw_1.Object.envase[1]
	If IsNull( li_envase) Then
		MessageBox( "Envase Erróneo", "Falta seleccionar un Envase.", StopSign!, Ok!)
		Return 1				 
   End If
End If

// Acepta Calidad //
If dw_1.Object.todoscal[1] = 1 Then
	ls_Calidad = '*'
	If dw_1.Object.conscal[1] = 1 Then
		ls_calidad ='**'
	End If
Else
	 ls_Calidad = dw_1.Object.Calidad[1]
	If IsNull( ls_Calidad) Then
		MessageBox( "Envase Erróneo", "Falta seleccionar un Envase.", StopSign!, Ok!)
		Return 1				 
   End If
End If

ld_fechadesde = DateTime(dw_1.Object.fechad[1])

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", StopSign!, Ok!)
	Return 1				 
End If

ls_fecha=mid(String(dw_1.Object.fechah[1],'dd/mm/yyyy'),1,10)
ld_fechahasta = DateTime(ls_fecha)

If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", StopSign!, Ok!)
	Return 1				 
End If

If ld_fechadesde > ld_fechahasta Then
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor o igual a la Fecha Desde.", StopSign!, Ok!)
	Return 1
End If	

If dw_1.Object.consfecha[1]= 1 Then	
	li_consfechad	 =		1
Else
	li_Consfechad	 =		0
End If

If dw_1.object.consguias[1] = 1 Then
	li_consguias 	=		1
Else
	li_consguias	=		0
End If

If dw_1.object.bodegas[1] = 1 Then
	li_bodega 	=		1
Else
	li_bodega	=		0
End If

vinf.dw_1.SetTransObject(sqlca)
li_Orden	=	dw_1.Object.orden[1]

If li_Orden = 1 Then
	//Como viene en la DW.
ElseIf li_Orden = 2 Then
	vinf.dw_1.SetSort("plde_codigo A, nombre A, codigo A, rut A, enva_tipoen A, enva_codigo A, cale_calida A, meen_fecmov A, tpmv_codigo A ")
	vinf.dw_1.Sort()
	vinf.dw_1.GroupCalc()
Else		
	vinf.dw_1.SetSort("plde_codigo A, rut A, codigo A, nombre A, enva_tipoen A, enva_codigo A, cale_calida A, meen_fecmov A, tpmv_codigo A ")
	vinf.dw_1.Sort()
	vinf.dw_1.GroupCalc()
End If

ll_Fila	=	vinf.dw_1.Retrieve(ll_Planta, ll_Productor,ll_plantadest,li_tipoenvase,li_envase,ld_Fechadesde,ld_fechaHasta,li_Consfechad,li_tipinf,&
									li_SaldoCero,li_TipoSaldo, li_ConsGuias,ls_Calidad, li_Grupo, li_bodega, li_Cliente)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_cta_cte_enva_prod
integer x = 2432
integer y = 980
integer taborder = 140
end type

type dw_1 from datawindow within w_info_cta_cte_enva_prod
integer x = 288
integer y = 408
integer width = 2025
integer height = 1220
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_ctacte_productor_seleccion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null
String	ls_Columna

SetNull(li_Null)

ls_Columna	=	dwo.Name

CHOOSE Case ls_Columna
	Case "cliente"
		If NOT iuo_Cliente.Existe(Integer(data),True,Sqlca) Then
			This.SetItem(Row, ls_Columna, li_Null )
			RETURN 1
		End If
		
	Case "todocliente"
		If data = '1' Then This.Object.cliente[row]		=	li_Null
				
	Case "planta"
		If NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) Then
			This.SetItem(1, "planta", li_Null )
			RETURN 1
		End If		
							
	Case "plantadesp"
		If NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) Then
			This.SetItem(1, "plantadesp", li_Null )
			RETURN 1
		End If
	
	Case "tiposenvase"
		If Not iuo_Tipo.Existe(Integer(data), True, Sqlca) Then
			This.SetItem(1, "tiposenvase", li_Null )
			This.Object.envase[row]	=	li_Null
	         dw_1.Object.todosenva.protect = 1
			This.SetFocus()
			RETURN 1
		Else
			This.Object.envase[row]	=	li_Null
			dw_1.Object.todosenva.protect = 0
		   	If idwc_envase.Retrieve(Integer(Data)) = 0 Then RETURN 1
		End If

	Case "envase"
		If Not iuo_Envases.Existe(This.Object.tiposenvase[Row],Integer(data), True, Sqlca) Then
			This.SetItem(1, "envase", li_Null )
			RETURN 1
		Else			
			This.Object.calidad[row] = String(li_Null)
			dw_1.Object.todoscal.protect = 0
			If idwc_calidad.Retrieve(dw_1.Object.tiposenvase[row],Integer(data)) = 0 Then
				MessageBox("Atención","Falta Registrar Calidades asociadas a los envases")
			End If
		End If
		
	Case "productor"
		If NOT iuo_Productores.Existe(Long(data),True,SqlCa) Then
			This.SetItem(1, "productor", li_Null)
			RETURN 1
		End If

	Case "todosplanta"
		If data = '1' Then
			This.Object.planta[row]		=	li_Null
		End If

	Case	"todostiposenva"
		If	data = '1' Then
			This.Object.tiposenvase[row]	= li_Null
			dw_1.Object.envase[row]		   = li_Null
			dw_1.Object.todosenva[row]	   = 1
			dw_1.Object.calidad[row]	 	= String(li_Null)
			dw_1.Object.todoscal[row]		= 1
		End If

	Case "todoscal"
		If data = '1' Then
			This.Object.calidad[row]		=	String(li_Null)
		End If
		
	Case	"todosprod"
		If this.Object.otrasplanta[row] = 0 Then
		  If	data = '1' Then This.Object.productor[row]	=	li_Null
		Else
 		  If	data = '1' Then This.Object.plantadesp[row]	=	li_Null
		End If
		
	Case	"todosenva"
		If	data = '1' Then 
			This.Object.envase[row]		= li_Null
			This.Object.todoscal[row] 	= 1
			This.Object.calidad[row]	= String(li_Null)
		End If
		
	Case  "consprod"
		If data = '1' Then
			This.Object.grupprod[row] = 0
		End If

	Case  "grupprod"
		If data = '1' Then
			This.Object.consprod[row] = 0
		End If
	
End CHOOSE
end event

event itemerror;Return 1
end event

event dberror;Return 1
end event

